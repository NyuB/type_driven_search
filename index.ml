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

  val init : config -> t
  val store : t -> CFunction.t list -> unit
  val get : t -> Signature.t -> CFunction.t list
end

module InMemory : S with type config = unit = struct
  type t = CFunction.t list ref
  type config = unit

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
  | Truncate
  | Keep

type config_open_file =
  { file : string
  ; mode : config_open_mode
  }

module FileBased : S with type config = config_open_file = struct
  type t = string
  type config = config_open_file

  let init ({ file; mode } : config) : t =
    let open_mode =
      match mode with
      | Keep -> [ Open_creat; Open_text; Open_wronly ]
      | Truncate -> [ Open_trunc; Open_creat; Open_wronly; Open_text ]
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
        (fun CFunction.{ name; signature } ->
           Printf.fprintf oc "%s:%s\n" name (Signature.string_of_t signature))
        list)
  ;;

  let get t signature =
    let signature = Signature.canonical signature in
    with_file_r t (fun ic ->
      let rec aux acc =
        match In_channel.input_line ic with
        | None -> List.rev acc
        | Some line ->
          let split = String.split_on_char ':' line in
          let n = List.hd split
          and s = List.nth split 1 |> Signature.parse |> Option.get in
          if Signature.equal (Signature.canonical s) signature
          then aux (CFunction.{ name = n; signature = s } :: acc)
          else aux acc
      in
      aux [])
  ;;
end
