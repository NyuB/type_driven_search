let or_failwith message = function
  | Some v -> v
  | None -> failwith message
;;

module CFunction = struct
  type t =
    { name : string
    ; signature : Signature.t
    }

  let name t = t.name
  let signature t = t.signature

  (* Comparisons *)
  include struct
    let compare (a : t) (b : t) =
      Compare.compare_by_each
        [ Compare.compare_by Signature.compare signature
        ; Compare.compare_by String.compare name
        ]
        a
        b
    ;;

    let equal a b = Compare.equality compare a b
  end

  let string_of_t { name; signature } =
    Printf.sprintf
      "%s %s(%s)"
      (Signature.Ctype.string_of_t signature.return)
      name
      (String.concat ", " (List.map Signature.Ctype.string_of_t signature.params))
  ;;
end

module type S = sig
  type t
  type config

  val id : string
  val init : config -> t
  val store : t -> CFunction.t list -> unit
  val get : t -> Signature.t -> CFunction.t list
end

module InMemory : S with type config = unit = struct
  type t = CFunction.t list ref
  type config = unit

  let id = "InMemory"
  let init () = ref []
  let store t list = t := List.append !t list

  let get t s : CFunction.t list =
    List.filter
      (Fun.compose
         (Signature.equal (Signature.canonical s))
         (Fun.compose Signature.canonical CFunction.signature))
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

module FunctionEntry = struct
  (* Fixed-size, fseekable entry *)
  let entry_size = 150

  let fitting_entry_size s =
    let l = String.length s in
    if l > entry_size - 1 (* Account for the final \n *)
    then failwith "Signature to long, complain to the developper"
    else Printf.sprintf "%s%s\n" s (String.make (entry_size - l - 1) ' ')
  ;;

  let write oc ({ name; signature } : CFunction.t) =
    let sigstr = Printf.sprintf "%s:%s" name (Signature.string_of_t signature) in
    Out_channel.output_string oc (fitting_entry_size sigstr);
    Out_channel.flush oc
  ;;

  let parse line : CFunction.t =
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

  let read ic =
    really_input_string ic entry_size
    |> fun line -> parse @@ String.sub line 0 (entry_size - 1)
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

  let store t list = with_file_w t (fun oc -> List.iter (FunctionEntry.write oc) list)

  let get t signature =
    let signature = Signature.canonical signature in
    with_file_r t (fun ic ->
      let rec aux acc =
        match In_channel.input_line ic with
        | None -> List.rev acc
        | Some line ->
          let f = FunctionEntry.parse line in
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
      let count = In_channel.input_line ic |> Option.get |> int_of_string in
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

  let with_file_r f fn =
    let ic = open_in_gen [ Open_binary; Open_creat; Open_nonblock ] 0o666 f in
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> fn ic)
  ;;

  let temp_file_for t =
    Filename.open_temp_file ~temp_dir:"." ~mode:[ Open_binary; Open_append ] t ".swp"
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

  let insert ic temp_oc (header : Header.t) f : unit =
    let rec aux pos =
      assert (pos <= header.count);
      if pos = header.count
      then FunctionEntry.write temp_oc f
      else (
        let f_at_line = FunctionEntry.read ic in
        if
          Signature.compare
            (Signature.canonical f_at_line.signature)
            (Signature.canonical f.signature)
          >= 0
        then (
          FunctionEntry.write temp_oc f;
          FunctionEntry.write temp_oc f_at_line;
          pipe_all ic temp_oc)
        else (
          FunctionEntry.write temp_oc f_at_line;
          aux (pos + 1)))
    in
    aux 0
  ;;

  (** swap_back t temp_t copies temp_t into t then deletes it *)
  let swap_back t temp_t header =
    let () =
      In_channel.with_open_bin temp_t
      @@ fun ic ->
      Out_channel.with_open_bin t
      @@ fun oc ->
      let () = Header.write header oc in
      let rec aux () =
        match In_channel.input_line ic with
        | Some line ->
          Out_channel.output_string oc line;
          Out_channel.output_char oc '\n';
          aux ()
        | None -> ()
      in
      aux ();
      Out_channel.flush oc
    in
    Sys.remove temp_t
  ;;

  let store_one t f : unit =
    let temp_t, temp_oc = temp_file_for t in
    Fun.protect ~finally:(fun () -> Out_channel.close_noerr temp_oc)
    @@ fun () ->
    with_file_r t
    @@ fun ic ->
    let header = Header.read ic in
    insert ic temp_oc header f;
    Out_channel.flush temp_oc;
    swap_back t temp_t { count = header.count + 1 }
  ;;

  let store t list = List.iter (store_one t) list

  let get t signature =
    let query_signature = Signature.canonical signature in
    with_file_r t (fun ic ->
      let _ = Header.read ic in
      let rec aux acc =
        match In_channel.input_line ic with
        | None -> List.rev acc
        | Some line ->
          let line_function = FunctionEntry.parse line in
          let compare_line_to_query =
            Signature.compare
              (Signature.canonical line_function.signature)
              query_signature
          in
          if compare_line_to_query = 0
          then aux (line_function :: acc)
          else if
            (* Since the signature are sorted, no need to continue the iteration if we reached a > signature*)
            compare_line_to_query > 0
          then List.rev acc
          else aux acc
      in
      aux [])
  ;;
end
