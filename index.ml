let or_failwith message = function
  | Some v -> v
  | None -> failwith message
;;

module type S = sig
  type t
  type config

  val id : string
  val init : config -> t
  val store : t -> Signature.CFunction.t list -> unit
  val get : t -> Signature.t -> Signature.CFunction.t list
end

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
    }

  let init ic entry_size = { ic; entry_size; start_pos = In_channel.pos ic }

  let offset t entry_index =
    Int64.add t.start_pos
    @@ Int64.mul (Int64.of_int entry_index) (Int64.of_int t.entry_size)
  ;;

  (** handy when you want to trim a newline from an entry ...
    NB: fix entry size should eliminate the need for newlines, they are kept only to ease debugging index files
    *)
  let really_input_entry_minus_one t i =
    In_channel.seek t.ic (offset t i);
    really_input_string t.ic (t.entry_size - 1)
  ;;

  let pipe_all_before t i oc =
    In_channel.seek t.ic t.start_pos;
    let buff_size = 2048 in
    let buff = Bytes.make buff_size '\x00' in
    let rec loop to_write =
      let writeable = Int64.min (Int64.of_int buff_size) to_write |> Int64.to_int in
      match In_channel.input t.ic buff 0 writeable with
      | 0 -> ()
      | n ->
        Out_channel.output oc buff 0 n;
        loop (Int64.sub to_write (Int64.of_int n))
    in
    let to_write = Int64.sub (offset t i) t.start_pos in
    loop to_write
  ;;

  let pipe_all ic oc =
    let buff_size = 2048 in
    let buff = Bytes.make buff_size '\x00' in
    let rec loop () =
      match In_channel.input ic buff 0 buff_size with
      | 0 -> ()
      | n ->
        Out_channel.output oc buff 0 n;
        loop ()
    in
    loop ()
  ;;

  let pipe_all_after t i oc =
    In_channel.seek t.ic (offset t i);
    pipe_all t.ic oc
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
end

module FileBasedSorted : S with type config = config_open_file = struct
  type t = string
  type config = config_open_file

  let id = "FileBasedSorted"

  module Header = struct
    type t = { count : int }

    let write t oc =
      Out_channel.output_string oc (string_of_int t.count);
      Out_channel.output_char oc '\n'
    ;;

    let read ic =
      let count =
        In_channel.input_line ic
        |> or_failwith "Could not read header line"
        |> int_of_string_opt
        |> or_failwith "Invalid header line"
      in
      { count }
    ;;

    let init = { count = 0 }
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
    then failwith "Signature to long, complain to the developper"
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

  let gte sa sb = Signature.compare sa sb >= 0
  let lte sa sb = Signature.compare sa sb <= 0

  let find_insertion_point reader count queried_signature =
    if count = 0
    then 0
    else (
      let high = count - 1 in
      let low_line =
        FixSizeEntryReader.really_input_entry_minus_one reader 0 |> FunctionRepr.parse
      in
      let high_line =
        FixSizeEntryReader.really_input_entry_minus_one reader high |> FunctionRepr.parse
      in
      if gte (Signature.canonical low_line.signature) queried_signature
      then 0
      else if lte (Signature.canonical high_line.signature) queried_signature
      then high + 1
      else (
        let rec loop low high =
          assert (high >= low);
          if low == high
          then low
          else if low = high - 1
          then high
          else (
            let middle = low + ((high - low) / 2) in
            let mid_line =
              FixSizeEntryReader.really_input_entry_minus_one reader middle
              |> FunctionRepr.parse
            in
            let compare =
              Signature.compare (Signature.canonical mid_line.signature) queried_signature
            in
            if compare = 0
            then middle
            else if compare > 0
            then (
              assert (middle != high);
              loop low middle)
            else (
              assert (middle != low);
              loop middle high))
        in
        loop 0 high))
  ;;

  let insert reader temp_oc (header : Header.t) (f : Signature.CFunction.t) : unit =
    let insertion_pos =
      find_insertion_point reader header.count (Signature.canonical f.signature)
    in
    FixSizeEntryReader.pipe_all_before reader insertion_pos temp_oc;
    output_cfunction temp_oc f;
    FixSizeEntryReader.pipe_all_after reader insertion_pos temp_oc
  ;;

  external mv : string -> string -> int = "mv"

  (** swap_back t temp_t copies temp_t into t then deletes it *)
  let swap_back t temp_t = ignore (mv temp_t t)

  let store_one t f : unit =
    let temp_t, temp_oc = temp_file_for t in
    Fun.protect ~finally:(fun () -> Out_channel.close_noerr temp_oc)
    @@ fun () ->
    with_file_r t
    @@ fun ic ->
    let header = Header.read ic in
    Header.write { count = header.count + 1 } temp_oc;
    insert (FixSizeEntryReader.init ic entry_size) temp_oc header f;
    Out_channel.flush temp_oc;
    swap_back t temp_t
  ;;

  let store t list = List.iter (store_one t) list

  let find_signature_position reader signature count =
    let low = 0
    and high = count in
    let rec loop low high =
      if low >= high
      then None
      else (
        let middle = low + ((high - low) / 2) in
        let mid_line =
          FixSizeEntryReader.really_input_entry_minus_one reader middle
          |> FunctionRepr.parse
        in
        let compare =
          Signature.compare (Signature.canonical mid_line.signature) signature
        in
        if compare = 0
        then Some middle
        else if compare > 0
        then loop low middle
        else loop (middle + 1) high)
    in
    loop low high
  ;;

  let all_around_position ~position ~count reader signature =
    let res = ref [] in
    let i = ref position in
    while !i >= 0 do
      let line =
        FixSizeEntryReader.really_input_entry_minus_one reader !i |> FunctionRepr.parse
      in
      if Signature.equal (Signature.canonical line.signature) signature
      then (
        res := line :: !res;
        i := !i - 1)
      else i := -1
    done;
    let j = ref (position + 1) in
    while !j < count do
      let line =
        FixSizeEntryReader.really_input_entry_minus_one reader !j |> FunctionRepr.parse
      in
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
      let Header.{ count } = Header.read ic in
      let reader = FixSizeEntryReader.init ic entry_size in
      match find_signature_position reader query_signature count with
      | None ->
        print_endline "Found no position";
        []
      | Some position -> all_around_position reader ~position ~count query_signature)
  ;;
end
