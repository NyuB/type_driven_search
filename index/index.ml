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

module Tag = struct
  type t = string

  let of_return return = Printf.sprintf "r:%s" @@ Signature.Ctype.string_of_t return

  let of_param n ctype =
    (* FIXME "%02d" keeps 2 digits handle > 99 (!) parameters or check C spec for the max ??? *)
    Printf.sprintf "p:%02d:%s" n (Signature.Ctype.string_of_t ctype)
  ;;

  let of_query_return (query : Query.t) : t = of_return query.return
  let of_query_param (p : Query.Param.t) : t = of_param p.count p.ctype
  let of_query (q : Query.t) = [ of_query_return q ] @ List.map of_query_param q.params

  let explode_query_param (param : Query.Param.t) =
    let rec aux acc (param : Query.Param.t) =
      match param.count with
      | 0 -> acc
      | n -> aux (of_query_param param :: acc) { param with count = n - 1 }
    in
    aux [] param
  ;;

  let of_signature s =
    let query = Query.condense_signature s in
    [ of_query_return query ]
    @ (query.params |> List.map explode_query_param |> List.flatten)
  ;;

  let compare = String.compare
  let equal = String.equal
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

let itol i = Int64.of_int i

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

module FixSizeEntryReader = struct
  type t =
    { ic : In_channel.t
    ; start_pos : int64
    ; entry_size : int
    ; count : int
    }

  let init ic ~entry_size ~count ~start_pos = { ic; entry_size; start_pos; count }

  let offset t entry_index =
    Int64.add t.start_pos @@ Int64.mul (itol entry_index) (itol t.entry_size)
  ;;

  let really_input_entry t i =
    In_channel.seek t.ic (offset t i);
    let buff = Bytes.create t.entry_size in
    really_input t.ic buff 0 t.entry_size;
    buff
  ;;

  let pipe_all_between t low_inclusive high_exclusive oc =
    let offset_low = offset t low_inclusive in
    In_channel.seek t.ic offset_low;
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
    let to_write = Int64.sub (offset t high_exclusive) offset_low in
    loop to_write
  ;;

  let pipe_all_before t i oc = pipe_all_between t 0 i oc
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

  let pipe_n t oc byte_count =
    In_channel.seek t.ic t.start_pos;
    pipe_n t.ic oc byte_count
  ;;

  let read_varlen_string t heap_offset =
    In_channel.seek t.ic (Int64.add t.start_pos heap_offset);
    let buff = Bytes.create var_len_size in
    In_channel.really_input t.ic buff 0 var_len_size
    |> or_failwith (Printf.sprintf "Unable to read length at heap offset %Ld" heap_offset);
    let var_len = Bytes.get_int32_le buff 0 in
    let str =
      In_channel.really_input_string t.ic (Int32.to_int var_len)
      |> or_failwith
           (Printf.sprintf "Invalid heap read range at (%Ld)<%ld>" heap_offset var_len)
    in
    var_len, str
  ;;

  let read_string t (heap_offset : heap_offset) =
    let _, s = read_varlen_string t heap_offset in
    s
  ;;

  (** [write_string oc s] writes [s] into [oc] prefixed by it's length as a 32 bits integer *)
  let write_string oc s =
    let buff = Bytes.create var_len_size in
    Bytes.set_int32_le buff 0 (Int32.of_int @@ String.length s);
    Out_channel.output oc buff 0 var_len_size;
    Out_channel.output_string oc s
  ;;

  let layout t count =
    let rec aux acc t n offset =
      if n >= count
      then List.rev acc
      else (
        let len, str = read_varlen_string t offset in
        let next_offset =
          Int64.add offset (Int64.add (itol var_len_size) (Int64.of_int32 len))
        in
        let repr = Printf.sprintf "[%04d](%04Ld)<%04ld>%s" n offset len str in
        aux (repr :: acc) t (n + 1) next_offset)
    in
    aux [] t 0 0L
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
    { functions_count : int
    ; heap_size : int64
    ; tags_index_count : int
    ; tags_count : int
    ; tag_heap_size : int64
    }

  let string_of_t
        { functions_count : int
        ; heap_size : int64
        ; tags_index_count : int
        ; tags_count
        ; tag_heap_size : int64
        }
    : string
    =
    Printf.sprintf
      "{ functions_count = %d; heap_size = %Ld; tags_index_count = %d; tags_count = %d; \
       tag_heap_size = %Ld }"
      functions_count
      heap_size
      tags_index_count
      tags_count
      tag_heap_size
  ;;

  let write t oc =
    Out_channel.output_string oc (string_of_int t.functions_count);
    Out_channel.output_char oc ':';
    Out_channel.output_string oc (Int64.to_string t.heap_size);
    Out_channel.output_char oc ':';
    Out_channel.output_string oc (string_of_int t.tags_index_count);
    Out_channel.output_char oc ':';
    Out_channel.output_string oc (string_of_int t.tags_count);
    Out_channel.output_char oc ':';
    Out_channel.output_string oc (Int64.to_string t.tag_heap_size);
    Out_channel.output_char oc '\n'
  ;;

  let read ic =
    let metadata =
      In_channel.input_line ic
      |> or_failwith "Could not read header line"
      |> String.split_on_char ':'
    in
    let functions_count =
      List.nth_opt metadata 0
      |> fun opt ->
      Option.bind opt int_of_string_opt
      |> or_failwith "Could not read functions count from header"
    in
    let heap_size =
      List.nth_opt metadata 1
      |> fun opt ->
      Option.bind opt Int64.of_string_opt
      |> or_failwith "Could not read heap size from header"
    in
    let tags_index_count =
      List.nth_opt metadata 2
      |> fun opt ->
      Option.bind opt int_of_string_opt
      |> or_failwith "Could not read tags index count from header"
    in
    let tags_count =
      List.nth_opt metadata 3
      |> fun opt ->
      Option.bind opt int_of_string_opt
      |> or_failwith "Could not read tags count from header"
    in
    let tag_heap_size =
      List.nth_opt metadata 4
      |> fun opt ->
      Option.bind opt Int64.of_string_opt
      |> or_failwith "Could not read tag heap size from header"
    in
    { functions_count; heap_size; tags_count; tags_index_count; tag_heap_size }
  ;;

  let init =
    { functions_count = 0
    ; heap_size = 0L
    ; tags_index_count = 0
    ; tags_count = 0
    ; tag_heap_size = 0L
    }
  ;;
end

(**
{v 
                  ||======
Header            ||count;heap_size;tag_count
                  ||======
Function index    ||<heap-offset(int64)> --------------
(sorted by sig)   ||...                                |
               ^  ||...                                |
        count  |  ||...                                |
          x    |  ||... ----------------------         |
       (8 + 4) |  ||...                       |        |
               |  ||...                       |        |
               |  ||...                       |        |
               |  ||...                       |        |
               v  ||...                       |        |
                  ||======                    v        |
Heap           ^  ||<entry 1 (var size)> <entry 2>     |
               |  ||... <entry n (heap-length)> <------/
     heap_size |  ||... ... ... ...         ^
               |  ||... ...                 | 
               v  ||... <entry count>       | 
                  ||=====       /-----------
                  ||======      |
Tag index         || <heap_offset><tag_heap_offset>
(sorted by tag)   ||...                       | 
                  ||...                       |
                  ||..                        |
Tag heap          || <tag 1 var_size>   <---- /
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
    ; tag_index_reader : FixSizeEntryReader.t
      (** A reader into the fixed-size tag index part of the store, to retrieve pointers to the heaps *)
    ; tag_heap_reader : Heap_Section.t
      (** A reader into the heap part of the store, to read arbitrary data *)
    }

  (* 64bits heap offset *)
  let entry_size = 8

  (** 2 x 64bits heap offsets *)
  let tag_entry_size = entry_size * 2

  let init ic =
    let header = Header.read ic in
    let index_reader =
      FixSizeEntryReader.init
        ic
        ~entry_size
        ~count:header.functions_count
        ~start_pos:(In_channel.pos ic)
    in
    let heap_reader = Heap_Section.init ic (FixSizeEntryReader.end_pos index_reader) in
    let tag_index_reader =
      FixSizeEntryReader.init
        ic
        ~entry_size:tag_entry_size
        ~count:header.tags_index_count
        ~start_pos:(Int64.add heap_reader.start_pos header.heap_size)
    in
    let tag_heap_reader =
      Heap_Section.init ic (FixSizeEntryReader.end_pos tag_index_reader)
    in
    { header; index_reader; heap_reader; tag_index_reader; tag_heap_reader }
  ;;

  let next_heap_offset t = t.header.heap_size
  let next_tag_heap_offset t = t.header.tag_heap_size

  (** for debug *)
  let _string_of_t
        { header; index_reader; heap_reader; tag_index_reader; tag_heap_reader }
    =
    Printf.sprintf
      "%s (%Ld -> %Ld) (%Ld ... ) (%Ld -> %Ld) (%Ld ...)"
      (Header.string_of_t header)
      index_reader.start_pos
      (FixSizeEntryReader.end_pos index_reader)
      heap_reader.start_pos
      tag_index_reader.start_pos
      (FixSizeEntryReader.end_pos tag_index_reader)
      tag_heap_reader.start_pos
  ;;
end

module FileBasedSorted = struct
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

  let input_function_offset (storage : Storage.t) (i : int) : Heap_Section.heap_offset =
    let buff = FixSizeEntryReader.really_input_entry storage.index_reader i in
    let heap_offset = Bytes.get_int64_le buff 0 in
    heap_offset
  ;;

  let output_tag oc tag_heap_offset function_heap_offset =
    let buff = Bytes.create Storage.tag_entry_size in
    Bytes.set_int64_le buff 0 tag_heap_offset;
    Bytes.set_int64_le buff Storage.entry_size function_heap_offset;
    Out_channel.output_bytes oc buff
  ;;

  let input_tag (storage : Storage.t) (i : int)
    : Heap_Section.heap_offset * Heap_Section.heap_offset
    =
    let buff = FixSizeEntryReader.really_input_entry storage.tag_index_reader i in
    let tag_heap_offset = Bytes.get_int64_le buff 0 in
    let function_heap_offset = Bytes.get_int64_le buff Storage.entry_size in
    tag_heap_offset, function_heap_offset
  ;;

  let input_stored_function (storage : Storage.t) i =
    input_function_offset storage i
    |> fun heap_offset ->
    Heap_Section.read_string storage.heap_reader heap_offset |> FunctionRepr.parse
  ;;

  let find_insertion_position (storage : Storage.t) queried_signature =
    Binary_search.insertion_index storage.header.functions_count (fun i ->
      let f_at_i =
        input_stored_function storage i
        |> Signature.CFunction.signature
        |> Signature.canonical
      in
      Signature.compare f_at_i queried_signature)
  ;;

  let input_stored_tag (storage : Storage.t) i =
    let bytes = FixSizeEntryReader.really_input_entry storage.tag_index_reader i in
    let tag_offset = Bytes.get_int64_le bytes 0 in
    Heap_Section.read_string storage.tag_heap_reader tag_offset
  ;;

  let find_tag_insertion_position (storage : Storage.t) tag =
    Binary_search.insertion_index storage.header.tags_index_count (fun i ->
      Tag.compare (input_stored_tag storage i) tag)
  ;;

  let tag_already_at (storage : Storage.t) tag tag_insertion_pos =
    if tag_insertion_pos >= storage.header.tags_index_count
    then None
    else (
      let tag_offset, _ = input_tag storage tag_insertion_pos in
      let tag_at_pos = Heap_Section.read_string storage.tag_heap_reader tag_offset in
      if Tag.equal tag_at_pos tag then Some tag_offset else None)
  ;;

  type tag_insertion =
    { insertion_position : int (** Index in the tag index *)
    ; heap_offset : int64 (** Offset in the tag heap *)
    ; heap_content : string option
    }

  type tag_insertion_plan =
    { insertions : tag_insertion list
    ; additional_heap_size : int64
    ; additional_tag_count : int
    }

  let plan_tag_insertion (storage : Storage.t) (tags : Tag.t list) : tag_insertion_plan =
    let start_heap_offset = Storage.next_tag_heap_offset storage in
    let rec aux tag_insertions next_tag_heap_offset additional_tag_count = function
      | [] ->
        { insertions = List.rev tag_insertions
        ; additional_heap_size = Int64.sub next_tag_heap_offset start_heap_offset
        ; additional_tag_count
        }
      | tag :: rest ->
        let insertion_position = find_tag_insertion_position storage tag in
        (match tag_already_at storage tag insertion_position with
         | None ->
           let tag_insertion =
             { insertion_position
             ; heap_offset = next_tag_heap_offset
             ; heap_content = Some tag
             }
           in
           aux
             (tag_insertion :: tag_insertions)
             (Int64.add
                next_tag_heap_offset
                (itol (String.length tag + Heap_Section.var_len_size)))
             (additional_tag_count + 1)
             rest
         | Some heap_offset ->
           let tag_insertion = { insertion_position; heap_offset; heap_content = None } in
           aux
             (tag_insertion :: tag_insertions)
             next_tag_heap_offset
             additional_tag_count
             rest)
    in
    aux [] start_heap_offset 0 (List.sort Tag.compare tags)
  ;;

  let execute_tag_insertion_plan (storage : Storage.t) { insertions; _ } oc =
    let function_offset = Storage.next_heap_offset storage in
    let last_insertion_index =
      List.fold_left
        (fun last_insertion_index (tag_insertion : tag_insertion) ->
           FixSizeEntryReader.pipe_all_between
             storage.tag_index_reader
             last_insertion_index
             tag_insertion.insertion_position
             oc;
           output_tag oc tag_insertion.heap_offset function_offset;
           tag_insertion.insertion_position)
        0
        insertions
    in
    FixSizeEntryReader.pipe_all_after storage.tag_index_reader last_insertion_index oc;
    Heap_Section.pipe_n storage.tag_heap_reader oc storage.header.tag_heap_size;
    List.iter
      (fun (tag_insertion : tag_insertion) ->
         match tag_insertion.heap_content with
         | None -> ()
         | Some tag -> Heap_Section.write_string oc tag)
      insertions
  ;;

  let insert (storage : Storage.t) temp_oc (f : Signature.CFunction.t) : unit =
    let insertion_pos =
      find_insertion_position storage (Signature.canonical f.signature)
    in
    let tags = Tag.of_signature f.signature in
    let tag_insertion_plan = plan_tag_insertion storage tags in
    let repr, repr_length = cfunction_stored_representation f in
    Header.write
      Header.
        { functions_count =
            storage.header.functions_count + 1 (* We inserted a function *)
        ; heap_size =
            Int64.add
              storage.header.heap_size
              (itol (repr_length + Heap_Section.var_len_size))
        ; tags_index_count = storage.header.tags_index_count + List.length tags
        ; tags_count = storage.header.tags_count + tag_insertion_plan.additional_tag_count
        ; tag_heap_size =
            Int64.add storage.header.tag_heap_size tag_insertion_plan.additional_heap_size
        }
      temp_oc;
    FixSizeEntryReader.pipe_all_before storage.index_reader insertion_pos temp_oc;
    output_index temp_oc (Storage.next_heap_offset storage);
    FixSizeEntryReader.pipe_all_after storage.index_reader insertion_pos temp_oc;
    Heap_Section.pipe_n storage.heap_reader temp_oc storage.header.heap_size;
    Heap_Section.write_string temp_oc repr;
    execute_tag_insertion_plan storage tag_insertion_plan temp_oc
  ;;

  external mv : string -> string -> unit = "mv"

  let repr_storage_layout (storage : Storage.t) =
    let functions =
      List.init storage.header.functions_count (fun i ->
        Printf.sprintf
          "[%04d]| %Ld // %s"
          i
          (input_function_offset storage i)
          (input_stored_function storage i |> Signature.CFunction.string_of_t))
    in
    let functions_heap =
      Heap_Section.layout storage.heap_reader storage.header.functions_count
      |> List.map (String.cat "     ||")
    in
    let tags_index =
      List.init storage.header.tags_index_count (fun i ->
        let tag_offset, function_offset = input_tag storage i in
        Printf.sprintf
          "[%04d]| %Ld %Ld // %s %s"
          i
          tag_offset
          function_offset
          (Heap_Section.read_string storage.tag_heap_reader tag_offset)
          (Heap_Section.read_string storage.heap_reader function_offset))
    in
    let tag_heap =
      Heap_Section.layout storage.tag_heap_reader storage.header.tags_count
      |> List.map (String.cat "     ||")
    in
    let separator = "     ||-----" in
    [ separator ]
    @ functions
    @ [ separator ]
    @ functions_heap
    @ [ separator ]
    @ tags_index
    @ [ separator ]
    @ tag_heap
    @ [ separator ]
  ;;

  let repr_layout t =
    with_file_r t
    @@ fun ic ->
    let storage = Storage.init ic in
    repr_storage_layout storage
  ;;

  let store_one t f : unit =
    let temp_t, temp_oc = temp_file_for t in
    Fun.protect ~finally:(fun () -> Out_channel.close_noerr temp_oc)
    @@ fun () ->
    with_file_r t
    @@ fun ic ->
    let storage = Storage.init ic in
    insert storage temp_oc f;
    Out_channel.flush temp_oc;
    mv temp_t t
  ;;

  let store t list = List.iter (store_one t) list

  let find_signature_position (storage : Storage.t) signature =
    Binary_search.index storage.header.functions_count (fun i ->
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
          ~count:storage.header.functions_count
          ~position
          (input_stored_function storage)
          (fun f -> Signature.equal (Signature.canonical f.signature) canonical))
  ;;

  let find_tag_position (storage : Storage.t) tag =
    Binary_search.index storage.header.tags_index_count (fun i ->
      let t = input_stored_tag storage i in
      Tag.compare t tag)
  ;;

  module FunctionOffsetSet = Set.Make (Int64)

  let inter_or_start_with set = function
    | None -> set
    | Some previous_set -> FunctionOffsetSet.inter set previous_set
  ;;

  let all_functions_tagged_with (storage : Storage.t) tag =
    match find_tag_position storage tag with
    | None -> FunctionOffsetSet.empty
    | Some position ->
      all_around_position
        ~count:storage.header.tags_index_count
        ~position
        (fun i -> input_tag storage i)
        (fun (tag_offset, _) ->
           let t = Heap_Section.read_string storage.tag_heap_reader tag_offset in
           Tag.equal t tag)
      |> List.fold_left
           (fun set (_, f_offset) -> FunctionOffsetSet.add f_offset set)
           FunctionOffsetSet.empty
  ;;

  let query t (q : Query.t) =
    with_file_r t (fun ic ->
      let storage = Storage.init ic in
      let tags = Tag.of_query q in
      let early_cut_on = storage.header.functions_count / 10 in
      let rec aux current_set = function
        | [] ->
          current_set
          |> Option.value ~default:FunctionOffsetSet.empty
          |> FunctionOffsetSet.to_list
          |> List.map (fun offset ->
            Heap_Section.read_string storage.heap_reader offset |> FunctionRepr.parse)
        | tag :: rest ->
          let matching = all_functions_tagged_with storage tag in
          let next = inter_or_start_with matching current_set in
          if FunctionOffsetSet.is_empty next
          then []
          else if FunctionOffsetSet.cardinal next < early_cut_on
          then
            (* Stop searching by tag and just scan linearly all functions *)
            FunctionOffsetSet.fold
              (fun offset acc ->
                 let f =
                   Heap_Section.read_string storage.heap_reader offset
                   |> FunctionRepr.parse
                 in
                 if matches_query q f then f :: acc else acc)
              next
              []
          else aux (Option.some @@ inter_or_start_with matching current_set) rest
      in
      aux None tags)
  ;;
end

module SqliteBased : S with type config = config_open_file = struct
  (** sqlite3 db connection *)
  type t

  let id = "SqliteBased"

  type config = config_open_file

  external sqlite3_open : string -> t = "caml_sqlite3_open"
  external _sqlite3_close : t -> unit = "caml_sqlite3_close"
  external sqlite3_exec : t -> string -> (string -> unit) -> unit = "caml_sqlite3_exec"
  external sqlite3_last_row_id : t -> int64 = "caml_sqlite3_last_row_id"

  let init (config : config) =
    match config.mode with
    | Connect -> sqlite3_open config.file
    | Create ->
      if Sys.file_exists config.file then Sys.remove config.file;
      let t = sqlite3_open config.file in
      sqlite3_exec
        t
        "create table functions(id integer primary key, repr varchar(500));"
        ignore;
      sqlite3_exec
        t
        "create table tags(id integer primary key, name varchar(500));"
        ignore;
      sqlite3_exec
        t
        {|
create table tag_to_function(
  id integer primary key,
  tag_id integer,
  function_id integer, 
  -- constraints
  foreign key (tag_id) references tags(id),
  foreign key (function_id) references functions(id)
);
        |}
        ignore;
      t
  ;;

  let store t fs =
    let insert_function_values =
      fs
      |> List.map (fun f -> Printf.sprintf "('%s')" (FunctionRepr.format f))
      |> String.concat ","
    in
    let insert_functions =
      Printf.sprintf "insert into functions (repr) values %s;" insert_function_values
    in
    let last_row_id = sqlite3_last_row_id t in
    sqlite3_exec t insert_functions ignore;
    assert (
      Int64.equal
        (Int64.sub (sqlite3_last_row_id t) last_row_id)
        (Int64.of_int (List.length fs)))
  ;;

  let get t signature =
    let signature = Signature.canonical signature in
    let select = "select repr from functions;" in
    let result = ref [] in
    let append =
      fun f ->
      let parsed = FunctionRepr.parse f in
      if Signature.equal signature (Signature.canonical parsed.signature)
      then result := parsed :: !result
      else ()
    in
    sqlite3_exec t select append;
    !result
  ;;

  let query t query =
    let select = "select repr from functions;" in
    let result = ref [] in
    sqlite3_exec t select (fun f ->
      let parsed = FunctionRepr.parse f in
      if matches_query query parsed then result := parsed :: !result else ());
    !result
  ;;
end
