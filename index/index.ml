let or_failwith message = function
  | Some v -> v
  | None -> failwith message
;;

module Query = struct
  module Param = struct
    type t =
      { count : int
      ; ctype : Signature.Ctype.t
      }

    let count t = t.count
    let ctype t = t.ctype

    let compare a b =
      Compare.compare_by_each
        [ Compare.compare_by Signature.Ctype.compare ctype
        ; Compare.compare_by Int.compare count
        ]
        a
        b
    ;;

    let equal = Compare.equality compare

    let string_of_t { count; ctype } =
      Printf.sprintf "(%d %s)" count (Signature.Ctype.string_of_t ctype)
    ;;
  end

  type t =
    { return : Signature.Ctype.t
    ; params : Param.t list
    }

  let string_of_t { return; params } =
    Printf.sprintf
      "((returns %s) %s)"
      (Signature.Ctype.string_of_t return)
      (params |> List.map Param.string_of_t |> String.concat " ")
  ;;

  let highest_count_first a b =
    Compare.compare_by
      Int.compare
      Param.count
      (* Intentionnal reverse *)
      b
      a
  ;;

  let condense_signature (signature : Signature.t) : t =
    let signature = Signature.canonical signature in
    let params =
      List.fold_left
        (fun (acc : Param.t list) ctype ->
           match acc with
           | { count; ctype = ct } :: tail when Signature.Ctype.equal ctype ct ->
             { count = count + 1; ctype } :: tail
           | any -> { count = 1; ctype } :: any)
        []
        signature.params
      |> List.sort highest_count_first
    in
    { return = signature.return; params }
  ;;
end

module type S = sig
  type t
  type config

  val id : string
  val init : config -> t
  val store : t -> Signature.CFunction.t list -> unit
  val get : t -> Signature.t -> Signature.CFunction.t list
  val query : t -> Query.t -> Signature.CFunction.t list
end

let matches_query (query : Query.t) (f : Signature.CFunction.t) =
  let condensed = Query.condense_signature f.signature in
  Signature.Ctype.equal condensed.return query.return
  && List.for_all
       (fun (queried : Query.Param.t) ->
          List.exists
            (fun (actual : Query.Param.t) ->
               Signature.Ctype.equal actual.ctype queried.ctype
               && actual.count >= queried.count)
            condensed.params)
       query.params
;;

module InMemory : S with type config = unit = struct
  type t = Signature.CFunction.t list ref
  type config = unit

  let id = "InMemory"
  let init () = ref []
  let store t list = t := List.append !t list

  let get t s : Signature.CFunction.t list =
    List.filter
      (Fun.compose
         (Signature.equal (Signature.canonical s))
         (Fun.compose Signature.canonical Signature.CFunction.signature))
      !t
  ;;

  let query (t : t) (query : Query.t) = List.find_all (matches_query query) !t
end

type config_open_mode =
  | Create
  | Connect

type config_open_file =
  { file : string
  ; mode : config_open_mode
  }

module FunctionRepr = struct
  let parse line : Signature.CFunction.t =
    let split = String.split_on_char ':' line in
    let name = List.hd split
    and sigstr =
      List.nth_opt split 1 |> or_failwith (Printf.sprintf "Invalid line '%s'" line)
    in
    let signature =
      sigstr
      |> Signature.parse
      |> or_failwith (Printf.sprintf "Invalid signature: '%s'" sigstr)
    in
    { name; signature }
  ;;

  let format (f : Signature.CFunction.t) =
    Printf.sprintf "%s:%s" f.name (Signature.string_of_t f.signature)
  ;;
end

module FixSizeEntryReader = struct
  type t =
    { ic : In_channel.t
    ; start_pos : int64
    ; entry_size : int
    ; count : int
    }

  let itol i = Int64.of_int i
  let init ic entry_size count = { ic; entry_size; start_pos = In_channel.pos ic; count }

  let offset t entry_index =
    Int64.add t.start_pos @@ Int64.mul (itol entry_index) (itol t.entry_size)
  ;;

  let really_input_entry t i =
    In_channel.seek t.ic (offset t i);
    really_input_string t.ic t.entry_size
  ;;

  let pipe_all_before t i oc =
    In_channel.seek t.ic t.start_pos;
    let buff_size = 2048 in
    let buff = Bytes.make buff_size '\x00' in
    let rec loop to_write =
      let writeable = Int64.min (itol buff_size) to_write |> Int64.to_int in
      match In_channel.input t.ic buff 0 writeable with
      | 0 -> ()
      | n ->
        Out_channel.output oc buff 0 n;
        loop (Int64.sub to_write (itol n))
    in
    let to_write = Int64.sub (offset t i) t.start_pos in
    loop to_write
  ;;

  let pipe_n ic oc byte_count =
    let buff_size = 2048 in
    let buff_size_byte = itol buff_size in
    let buff = Bytes.make buff_size '\x00' in
    let rec loop n =
      if n = 0L
      then ()
      else (
        let to_write = Int64.min buff_size_byte n |> Int64.to_int in
        match In_channel.really_input ic buff 0 to_write with
        | None -> failwith "Write error"
        | Some () ->
          Out_channel.output oc buff 0 to_write;
          loop (Int64.sub n (Int64.of_int to_write)))
    in
    loop byte_count
  ;;

  let end_pos t = Int64.add t.start_pos (Int64.mul (itol t.count) (itol t.entry_size))

  let pipe_all_entries_after t i oc =
    let origin = offset t i
    and destination = end_pos t in
    In_channel.seek t.ic (offset t i);
    pipe_n t.ic oc (Int64.sub destination origin)
  ;;

  let pipe_heap t oc =
    In_channel.seek t.ic (end_pos t);
    let buff_size = 2048 in
    let buff = Bytes.make buff_size '\x00' in
    let rec loop () =
      match In_channel.input t.ic buff 0 buff_size with
      | 0 -> ()
      | n ->
        Out_channel.output oc buff 0 n;
        loop ()
    in
    loop ()
  ;;

  let _read_heap t heap_offset heap_len =
    In_channel.seek t.ic (Int64.add (end_pos t) heap_offset);
    In_channel.really_input_string t.ic heap_len
    |> or_failwith
         (Printf.sprintf "Invalid heap read range at index %Ld:%d" heap_offset heap_len)
  ;;
end

module FileBased : S with type config = config_open_file = struct
  type t = string
  type config = config_open_file

  let id = "FileBased"

  let init ({ file; mode } : config) : t =
    let open_mode =
      match mode with
      | Connect -> [ Open_creat; Open_text; Open_wronly ]
      | Create -> [ Open_trunc; Open_creat; Open_wronly; Open_text ]
    in
    let _ = open_out_gen open_mode 0o666 file in
    file
  ;;

  let with_file_w f fn =
    let oc = open_out_gen [ Out_channel.Open_append; Out_channel.Open_creat ] 0o666 f in
    Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> fn oc)
  ;;

  let with_file_r f fn =
    let ic = open_in f in
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> fn ic)
  ;;

  let store t list =
    with_file_w t (fun oc ->
      List.iter
        (fun (f : Signature.CFunction.t) ->
           Out_channel.output_string oc @@ FunctionRepr.format f;
           Out_channel.output_char oc '\n')
        list)
  ;;

  let get t signature =
    let signature = Signature.canonical signature in
    with_file_r t (fun ic ->
      let rec aux acc =
        match In_channel.input_line ic with
        | None -> List.rev acc
        | Some line ->
          let f = FunctionRepr.parse line in
          if Signature.equal (Signature.canonical f.signature) signature
          then aux (f :: acc)
          else aux acc
      in
      aux [])
  ;;

  let query t query =
    with_file_r t (fun ic ->
      let rec aux acc =
        match In_channel.input_line ic with
        | None -> List.rev acc
        | Some line ->
          let f = FunctionRepr.parse line in
          if matches_query query f then aux (f :: acc) else aux acc
      in
      aux [])
  ;;
end

(**
{v 
              ||======
Header        ||count;heap_size
              ||======
Index      ^  ||<heap-offset(int64)> <heap-length(int32)> ---
           |  ||...                                          |
    count  |  ||...                                          |
      x    |  ||... ----------------------                   |
   (8 + 4) |  ||...                       |                  |
           |  ||...                       |                  |
           |  ||...                       |                  |
           |  ||...                       |                  |
           v  ||...                       |                  |
              ||======                    v                  |
Heap          ||<entry 1 (var size)> <entry2>                |
              ||... <entry n (heap-length)> <----------------/
              ||... <entry count>

v}
*)
module FileBasedSorted : S with type config = config_open_file = struct
  type t = string
  type config = config_open_file

  let id = "FileBasedSorted"

  module Header = struct
    type t =
      { count : int
      ; heap_size : int64
      }

    let write t oc =
      Out_channel.output_string oc (string_of_int t.count);
      Out_channel.output_char oc ':';
      Out_channel.output_string oc (Int64.to_string t.heap_size);
      Out_channel.output_char oc '\n'
    ;;

    let read ic =
      let metadata =
        In_channel.input_line ic
        |> or_failwith "Could not read header line"
        |> String.split_on_char ':'
      in
      let count =
        List.nth_opt metadata 0
        |> fun opt ->
        Option.bind opt int_of_string_opt
        |> or_failwith "Could not read count from header"
      in
      let heap_size =
        List.nth_opt metadata 1
        |> fun opt ->
        Option.bind opt Int64.of_string_opt
        |> or_failwith "Could not read heap size from header"
      in
      { count; heap_size }
    ;;

    let init = { count = 0; heap_size = 0L }
  end

  let init ({ file; mode } : config) : t =
    match mode with
    | Connect -> file
    | Create ->
      let open_mode = [ Open_trunc; Open_creat; Open_wronly ] in
      let oc = open_out_gen open_mode 0o666 file in
      Header.write Header.init oc;
      Out_channel.flush oc;
      Out_channel.close oc;
      file
  ;;

  (* Fixed-size, fseekable entry *)
  let entry_size = 150

  let fitting_entry_size s =
    let l = String.length s in
    if l > entry_size - 1 (* Account for the final \n *)
    then
      failwith
        (Printf.sprintf
           "Entry '%s ... %s' too long (%d/%d)"
           (String.sub s 0 15)
           (String.sub s (l - 16) 15)
           l
           (entry_size - 1))
    else Printf.sprintf "%s%s\n" s (String.make (entry_size - l - 1) ' ')
  ;;

  let output_cfunction oc ({ name; signature } : Signature.CFunction.t) =
    let sigstr = Printf.sprintf "%s:%s" name (Signature.string_of_t signature) in
    Out_channel.output_string oc (fitting_entry_size sigstr)
  ;;

  let with_file_r f fn =
    let ic = open_in_gen [ Open_binary; Open_creat; Open_nonblock ] 0o666 f in
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> fn ic)
  ;;

  let temp_file_for t =
    Filename.open_temp_file ~temp_dir:"." ~mode:[ Open_binary; Open_append ] t ".swp"
  ;;

  let find_insertion_position reader count queried_signature =
    Binary_search.insertion_index count (fun i ->
      let f_at_i =
        FixSizeEntryReader.really_input_entry reader i
        |> FunctionRepr.parse
        |> Signature.CFunction.signature
        |> Signature.canonical
      in
      Signature.compare f_at_i queried_signature)
  ;;

  let insert reader temp_oc (header : Header.t) (f : Signature.CFunction.t) : unit =
    let insertion_pos =
      find_insertion_position reader header.count (Signature.canonical f.signature)
    in
    FixSizeEntryReader.pipe_all_before reader insertion_pos temp_oc;
    output_cfunction temp_oc f;
    FixSizeEntryReader.pipe_all_entries_after reader insertion_pos temp_oc;
    FixSizeEntryReader.pipe_heap reader temp_oc
  ;;

  external mv : string -> string -> unit = "mv"

  (** swap_back t temp_t copies temp_t into t then deletes it *)
  let swap_back t temp_t = ignore (mv temp_t t)

  let store_one t f : unit =
    let temp_t, temp_oc = temp_file_for t in
    Fun.protect ~finally:(fun () -> Out_channel.close_noerr temp_oc)
    @@ fun () ->
    with_file_r t
    @@ fun ic ->
    let header = Header.read ic in
    Header.write { count = header.count + 1; heap_size = header.heap_size } temp_oc;
    insert (FixSizeEntryReader.init ic entry_size header.count) temp_oc header f;
    Out_channel.flush temp_oc;
    swap_back t temp_t
  ;;

  let store t list = List.iter (store_one t) list

  let find_signature_position (reader : FixSizeEntryReader.t) signature =
    Binary_search.index reader.count (fun i ->
      let f_at_i =
        FixSizeEntryReader.really_input_entry reader i
        |> FunctionRepr.parse
        |> Signature.CFunction.signature
        |> Signature.canonical
      in
      Signature.compare f_at_i signature)
  ;;

  let all_around_position ~position ~count reader signature =
    let res = ref [] in
    let i = ref position in
    while !i >= 0 do
      let line = FixSizeEntryReader.really_input_entry reader !i |> FunctionRepr.parse in
      if Signature.equal (Signature.canonical line.signature) signature
      then (
        res := line :: !res;
        i := !i - 1)
      else i := -1
    done;
    let j = ref (position + 1) in
    while !j < count do
      let line = FixSizeEntryReader.really_input_entry reader !j |> FunctionRepr.parse in
      if Signature.equal (Signature.canonical line.signature) signature
      then (
        res := line :: !res;
        j := !j + 1)
      else j := count
    done;
    !res
  ;;

  let get t signature =
    let query_signature = Signature.canonical signature in
    with_file_r t (fun ic ->
      let Header.{ count; _ } = Header.read ic in
      let reader = FixSizeEntryReader.init ic entry_size count in
      match find_signature_position reader query_signature with
      | None -> []
      | Some position -> all_around_position reader ~position ~count query_signature)
  ;;

  (** FIXME Assumes functions are sorted by return-type first, this won't scale well ... *)
  let find_return_type_position (reader : FixSizeEntryReader.t) return =
    Binary_search.index reader.count (fun i ->
      let r_at_i =
        FixSizeEntryReader.really_input_entry reader i
        |> FunctionRepr.parse
        |> Signature.CFunction.signature
        |> Signature.return
      in
      Signature.Ctype.compare r_at_i return)
  ;;

  let all_return_around_position ~position ~count reader return =
    let res = ref [] in
    let i = ref position in
    while !i >= 0 do
      let line = FixSizeEntryReader.really_input_entry reader !i |> FunctionRepr.parse in
      if Signature.Ctype.equal line.signature.return return
      then (
        res := line :: !res;
        i := !i - 1)
      else i := -1
    done;
    let j = ref (position + 1) in
    while !j < count do
      let line = FixSizeEntryReader.really_input_entry reader !j |> FunctionRepr.parse in
      if Signature.Ctype.equal line.signature.return return
      then (
        res := line :: !res;
        j := !j + 1)
      else j := count
    done;
    !res
  ;;

  let query t (q : Query.t) =
    with_file_r t (fun ic ->
      let Header.{ count; _ } = Header.read ic in
      let reader = FixSizeEntryReader.init ic entry_size count in
      match find_return_type_position reader q.return with
      | None -> []
      | Some position ->
        all_return_around_position reader ~position ~count q.return
        |> List.filter (matches_query q))
  ;;
end
