module Ctype = struct
  type t =
    | Atom of string
    | Const of t
    | Pointer of t

  let rec equal a b =
    match a, b with
    | Atom sa, Atom sb -> String.equal sa sb
    | Pointer a, Pointer b | Const a, Const b -> equal a b
    | _ -> false
  ;;

  let atom a = Atom a
  let pointer a = Pointer a

  let pointer_n a n =
    let rec aux n t = if n <= 0 then t else aux (n - 1) (Pointer t) in
    aux n (Atom a)
  ;;

  let qualifier_parser =
    let open Parsers in
    discard whitespaces |> flat_map (fun () -> first_of [ keyword "*"; keyword "const" ])
  ;;

  let qualify t qualifiers =
    List.fold_left
      (fun t qualifier ->
         match qualifier with
         | "*" -> Pointer t
         | "const" -> Const t
         | _ -> failwith (String.cat "Unknown qualifier: " qualifier))
      t
      qualifiers
  ;;

  let unsigned =
    let open Parsers in
    keyword "unsigned "
    |. whitespaces
    |* first_of (List.map keyword [ "char"; "int"; "long"; "short" ])
    |/ fun (u, t) -> String.cat u t
  ;;

  let parser : t Parsers.t =
    let open Parsers in
    discard whitespaces
    ||> longest_of [ identifier; unsigned ]
    |. whitespaces
    |* zero_or_more qualifier_parser
    |/ fun (id, stars) -> qualify (Atom id) stars
  ;;

  let parse param =
    Parsers.parse_full parser param
    |> function
    | Some t -> t
    | None -> failwith (String.cat "Unable to parse type: " param)
  ;;

  let rec string_of_t = function
    | Atom s -> s
    | Const t ->
      let ts = string_of_t t in
      String.cat ts " const"
    | Pointer p ->
      let ps = string_of_t p in
      String.cat ps "*"
  ;;

  let rec explain requires_prefix = function
    | Atom a ->
      let prefix =
        if requires_prefix
        then (
          match String.get a 0 with
          | 'a' | 'e' | 'i' | 'o' | 'u' -> "an "
          | _ -> "a ")
        else ""
      in
      String.cat prefix a
    | Pointer t ->
      Printf.sprintf
        "%spointer to %s"
        (if requires_prefix then "a " else "")
        (explain true t)
    | Const t ->
      Printf.sprintf
        "%simmutable %s"
        (if requires_prefix then "an " else "")
        (explain false t)
  ;;

  let explain t = explain true t
end

type t =
  { return : Ctype.t
  ; params : Ctype.t list
  }

let string_of_t t =
  Printf.sprintf
    "%s (%s)"
    (Ctype.string_of_t t.return)
    (String.concat "," (List.map Ctype.string_of_t t.params))
;;

let equal a b = Ctype.equal a.return b.return && List.equal Ctype.equal a.params b.params

let trim_last_paren params =
  let params = String.trim params in
  let removed =
    if String.ends_with ~suffix:")" params
    then String.sub params 0 (String.length params - 1)
    else params
  in
  String.trim removed
;;

let remove_empty l = List.filter (fun s -> String.length s > 0) l

let parser =
  let open Parsers in
  let sep = discard whitespaces |>> (fun () -> keyword ",") |. whitespaces in
  let prefix = discard whitespaces |>> (fun () -> keyword "(") |. whitespaces
  and suffix = discard whitespaces |>> (fun () -> keyword ")") |. whitespaces in
  discard whitespaces
  ||> Ctype.parser
  |* list ~prefix ~suffix ~sep Ctype.parser
  |/ fun (return, params) -> { return; params }
;;

let parse str : t option = Parsers.parse_full parser str

let explain t =
  Printf.sprintf
    "a function returning %s from (%s)"
    (Ctype.explain t.return)
    (String.concat ", " (List.map Ctype.explain t.params))
;;
