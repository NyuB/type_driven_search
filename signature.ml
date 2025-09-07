module Ctype = struct
  type t =
    | Atom of string
    | Const of t
    | Pointer of t
    | Var_Args

  (* Comparisons *)
  include struct
    let rec compare a b : int =
      match a, b with
      (* Handle all 'same tag' cases*)
      | Atom sa, Atom sb -> String.compare sa sb
      | Pointer a, Pointer b | Const a, Const b -> compare a b
      | Var_Args, Var_Args -> 0
      (* Handle all 'differing tag' cases, descending order from Atom *)
      | Atom _, _ -> 1
      | _, Atom _ -> -1
      | Const _, _ -> 1
      | _, Const _ -> -1
      | Pointer _, _ -> 1
      | _, Pointer _ -> -1
    ;;

    let equal a b = Compare.equality a b
  end

  let atom a = Atom a
  let pointer a = Pointer a
  let const a = Const a

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

  module StringSet = Set.Make (String)

  let reserved_keywords =
    StringSet.of_list
      [ "auto"
      ; "class"
      ; "const"
      ; "do"
      ; "decltype"
      ; "else"
      ; "final"
      ; "for"
      ; "if"
      ; "override"
      ; "struct"
      ; "template"
      ; "typedef"
      ; "typename"
      ; "unsigned"
      ; "virtual"
      ; "while"
      ]
  ;;

  let reject_reserved_keywords (value : string) : string Parsers.t =
    if StringSet.mem value reserved_keywords
    then Fun.const None
    else fun input -> Some (input, value)
  ;;

  let parser : t Parsers.t =
    let open Parsers in
    discard whitespaces
    ||> longest_of [ identifier; unsigned; keyword "..." ]
    |>> reject_reserved_keywords
    |. whitespaces
    |* zero_or_more qualifier_parser
    |/ fun (id, stars) ->
    qualify (if String.equal id "..." then Var_Args else Atom id) stars
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
    | Var_Args -> "..."
  ;;

  let rec explain requires_prefix =
    let prefix s = if requires_prefix then s else "" in
    function
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
    | Pointer t -> Printf.sprintf "%spointer to %s" (prefix "a ") (explain true t)
    | Const t -> Printf.sprintf "%simmutable %s" (prefix "an ") (explain false t)
    | Var_Args -> Printf.sprintf "%svariadic number of arguments" (prefix "a ")
  ;;

  let explain t = explain true t
end

type t =
  { return : Ctype.t
  ; params : Ctype.t list
  }

let return t = t.return
let params t = t.params

let string_of_t t =
  Printf.sprintf
    "%s (%s)"
    (Ctype.string_of_t t.return)
    (String.concat "," (List.map Ctype.string_of_t t.params))
;;

include struct
  let compare (a : t) (b : t) =
    Compare.compare_by_each
      [ Compare.compare_by Ctype.compare return
      ; Compare.compare_by (List.compare Ctype.compare) params
      ]
      a
      b
  ;;

  let equal a b = Compare.equality compare a b
end

let canonical { return; params } = { return; params = List.sort Ctype.compare params }

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

module CFunction = struct
  type s = t

  type t =
    { name : string
    ; signature : s
    }

  let name t = t.name
  let signature t = t.signature

  (* Comparisons *)
  include struct
    let compare (a : t) (b : t) =
      Compare.compare_by_each
        [ Compare.compare_by compare signature; Compare.compare_by String.compare name ]
        a
        b
    ;;

    let equal a b = Compare.equality compare a b
  end

  let parser =
    let open Parsers in
    Ctype.parser
    |. whitespaces
    |* identifier
    |. whitespaces
    |** list
          ~prefix:(keyword "(")
          ~suffix:(keyword ")")
          ~sep:(whitespaces |. keyword ",")
          (whitespaces ||> Ctype.parser)
    |. whitespaces
    |. keyword ";"
    |/ fun (return, name, params) -> { name; signature = { return; params } }
  ;;

  let parse s = Parsers.parse_full parser s

  let string_of_t { name; signature } =
    Printf.sprintf
      "%s %s(%s)"
      (Ctype.string_of_t signature.return)
      name
      (String.concat ", " (List.map Ctype.string_of_t signature.params))
  ;;
end
