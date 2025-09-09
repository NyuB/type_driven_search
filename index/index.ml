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

let all_around_position ~position ~count get eq =
  let res = ref [] in
  let i = ref position in
  while !i >= 0 do
    let elem = get !i in
    if eq elem
    then (
      res := elem :: !res;
      i := !i - 1)
    else i := -1
  done;
  let j = ref (position + 1) in
  while !j < count do
    let elem = get !j in
    if eq elem
    then (
      res := elem :: !res;
      j := !j + 1)
    else j := count
  done;
  !res
;;

module InMemory : S with type config = unit = struct
  type t = Signature.CFunction.t Dynarray.t
  type config = unit

  let id = "InMemory"
  let init () = Dynarray.init 0 (fun _ -> failwith "Unreachable")

  let store (t : t) list =
    Dynarray.append_list t list;
    let arr = Dynarray.to_array t in
    Array.sort
      (fun (fa : Signature.CFunction.t) (fb : Signature.CFunction.t) ->
         Signature.compare
           (Signature.canonical fa.signature)
           (Signature.canonical fb.signature))
      arr;
    Dynarray.reset t;
    Dynarray.append_array t arr
  ;;

  let get (t : t) s : Signature.CFunction.t list =
    let canonical = Signature.canonical s in
    match
      Binary_search.index (Dynarray.length t) (fun i ->
        Signature.compare (Signature.canonical (Dynarray.get t i).signature) canonical)
    with
    | None -> []
    | Some position ->
      all_around_position ~position ~count:(Dynarray.length t) (Dynarray.get t) (fun f ->
        Signature.equal (Signature.canonical f.signature) canonical)
  ;;

  let query (t : t) (query : Query.t) =
    Dynarray.filter (matches_query query) t |> Dynarray.to_list
  ;;
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
  let init ic ~entry_size ~count start_pos = { ic; entry_size; start_pos; count }

  let offset t entry_index =
    Int64.add t.start_pos @@ Int64.mul (itol entry_index) (itol t.entry_size)
  ;;

  let really_input_entry t i =
    In_channel.seek t.ic (offset t i);
    let buff = Bytes.create t.entry_size in
    really_input t.ic buff 0 t.entry_size;
    buff
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

  let pipe_all_after t i oc =
    let origin = offset t i
    and destination = end_pos t in
    In_channel.seek t.ic (offset t i);
    pipe_n t.ic oc (Int64.sub destination origin)
  ;;
end

module Heap_Section = struct
  (** An offset into the heap *)
  type heap_offset = int64

  (** int32 *)
  let var_len_size = 4

  (** byte range from a file position to it's end *)
  type t =
    { ic : In_channel.t
    ; start_pos : int64
    }

  let init ic start_pos = { ic; start_pos }

  let pipe_all t oc =
    In_channel.seek t.ic t.start_pos;
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

  let read_string t (heap_offset : heap_offset) =
    In_channel.seek t.ic (Int64.add t.start_pos heap_offset);
    let buff = Bytes.create var_len_size in
    In_channel.really_input t.ic buff 0 var_len_size
    |> or_failwith (Printf.sprintf "Unable to read length at heap offset %Ld" heap_offset);
    let heap_len = Bytes.get_int32_le buff 0 in
    In_channel.really_input_string t.ic (Int32.to_int heap_len)
    |> or_failwith
         (Printf.sprintf "Invalid heap read range at index %Ld:%ld" heap_offset heap_len)
  ;;

  (** [write_string oc s] writes [s] into [oc] prefied by it's length as a 32 bits integer *)
  let write_string oc s =
    let buff = Bytes.create var_len_size in
    Bytes.set_int32_le buff 0 (Int32.of_int @@ String.length s);
    Out_channel.output oc buff 0 var_len_size;
    Out_channel.output_string oc s
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

module Header = struct
  type t =
    { count : int
    ; heap_size : int64
    }

  let one_more t length =
    { count = t.count + 1; heap_size = Int64.add t.heap_size (Int64.of_int length) }
  ;;

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
      Option.bind opt int_of_string_opt |> or_failwith "Could not read count from header"
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

(**
{v 
                  ||======
Header            ||count;heap_size;tag_count
                  ||======
Index          ^  ||<heap-offset(int64)> --------------
               |  ||...                                |
        count  |  ||...                                |
          x    |  ||... ----------------------         |
       (8 + 4) |  ||...                       |        |
               |  ||...                       |        |
               |  ||...                       |        |
               |  ||...                       |        |
               v  ||...                       |        |
                  ||======                    |        |
Tag index         || <heap_offset><tag_heap_offset>    |
(TODO)            ||...                       |        |
                  ||...                       |        |
                  ||...                       |        |
                  ||======                    v        |
Heap           ^  ||<entry 1 (var size)> <entry 2>     |
               |  ||... <entry n (heap-length)> <------/
     heap_size |  ||... ... ... ...
               |  ||... ...
               v  ||... <entry count>
                  ||=====
Tag heap          || <tag 1 var_size>   
(TOOD)
v}
Where:
- heap_offset, identify an offset from the start of the heap
- `entry n` is the nth function entry stored, variable sized represented as [<length(int32)><data[length]>]
- Index is a sorted section (by signature) of fixed-size pointers to a function in the heap
- Tag index is a sorted section (by tag) of pointer pairs, one to the heap and one to the tag heap.
*)
module Storage = struct
  type t =
    { header : Header.t (** Storage metadata *)
    ; index_reader : FixSizeEntryReader.t
      (** A reader into the fixed-size index part of the store, to retrieve pointers to the heap *)
    ; heap_reader : Heap_Section.t
      (** A reader into the heap part of the store, to read arbitrary data *)
    }

  (* 64bits heap offset *)
  let entry_size = 8

  let init ic =
    let header = Header.read ic in
    let index_reader =
      FixSizeEntryReader.init ic ~entry_size ~count:header.count (In_channel.pos ic)
    in
    let heap_reader = Heap_Section.init ic (FixSizeEntryReader.end_pos index_reader) in
    { header; index_reader; heap_reader }
  ;;
end

module FileBasedSorted : S with type config = config_open_file = struct
  type t = string
  type config = config_open_file

  let id = "FileBasedSorted"

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

  let cfunction_stored_representation ({ name; signature } : Signature.CFunction.t) =
    let repr = Printf.sprintf "%s:%s" name (Signature.string_of_t signature) in
    repr, String.length repr
  ;;

  let with_file_r f fn =
    let ic = open_in_gen [ Open_binary; Open_creat; Open_nonblock ] 0o666 f in
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> fn ic)
  ;;

  let temp_file_for t =
    Filename.open_temp_file ~temp_dir:"." ~mode:[ Open_binary; Open_append ] t ".swp"
  ;;

  let output_index oc heap_offset =
    let buff = Bytes.create Storage.entry_size in
    Bytes.set_int64_le buff 0 heap_offset;
    Out_channel.output_bytes oc buff
  ;;

  let input_index (storage : Storage.t) i : Heap_Section.heap_offset =
    let buff = FixSizeEntryReader.really_input_entry storage.index_reader i in
    let heap_offset = Bytes.get_int64_le buff 0 in
    heap_offset
  ;;

  let input_stored_function (storage : Storage.t) i =
    input_index storage i
    |> fun heap_offset ->
    Heap_Section.read_string storage.heap_reader heap_offset |> FunctionRepr.parse
  ;;

  let find_insertion_position (storage : Storage.t) queried_signature =
    Binary_search.insertion_index storage.header.count (fun i ->
      let f_at_i =
        input_stored_function storage i
        |> Signature.CFunction.signature
        |> Signature.canonical
      in
      Signature.compare f_at_i queried_signature)
  ;;

  let insert (storage : Storage.t) temp_oc (f : Signature.CFunction.t) heap_offset : unit =
    let insertion_pos =
      find_insertion_position storage (Signature.canonical f.signature)
    in
    FixSizeEntryReader.pipe_all_before storage.index_reader insertion_pos temp_oc;
    output_index temp_oc heap_offset;
    FixSizeEntryReader.pipe_all_after storage.index_reader insertion_pos temp_oc;
    Heap_Section.pipe_all storage.heap_reader temp_oc
  ;;

  external mv : string -> string -> unit = "mv"

  let store_one t f : unit =
    let temp_t, temp_oc = temp_file_for t in
    Fun.protect ~finally:(fun () -> Out_channel.close_noerr temp_oc)
    @@ fun () ->
    with_file_r t
    @@ fun ic ->
    let storage = Storage.init ic in
    let header = storage.header in
    let repr, repr_length = cfunction_stored_representation f in
    Header.write
      (Header.one_more header (repr_length + Heap_Section.var_len_size))
      temp_oc;
    insert storage temp_oc f header.heap_size;
    Heap_Section.write_string temp_oc repr;
    Out_channel.flush temp_oc;
    mv temp_t t
  ;;

  let store t list = List.iter (store_one t) list

  let find_signature_position (storage : Storage.t) signature =
    Binary_search.index storage.header.count (fun i ->
      let f_at_i =
        input_stored_function storage i
        |> Signature.CFunction.signature
        |> Signature.canonical
      in
      Signature.compare f_at_i signature)
  ;;

  let get t signature =
    let canonical = Signature.canonical signature in
    with_file_r t (fun ic ->
      let storage = Storage.init ic in
      match find_signature_position storage canonical with
      | None -> []
      | Some position ->
        all_around_position
          ~count:storage.header.count
          ~position
          (input_stored_function storage)
          (fun f -> Signature.equal (Signature.canonical f.signature) canonical))
  ;;

  (** FIXME Assumes functions are sorted by return-type first, this won't scale well ... *)
  let find_return_type_position (storage : Storage.t) return =
    Binary_search.index storage.header.count (fun i ->
      let r_at_i =
        input_stored_function storage i
        |> Signature.CFunction.signature
        |> Signature.return
      in
      Signature.Ctype.compare r_at_i return)
  ;;

  let query t (q : Query.t) =
    with_file_r t (fun ic ->
      let storage = Storage.init ic in
      match find_return_type_position storage q.return with
      | None -> []
      | Some position ->
        all_around_position
          ~count:storage.header.count
          ~position
          (input_stored_function storage)
          (fun f -> Signature.Ctype.equal f.signature.return q.return)
        |> List.filter (matches_query q))
  ;;
end
