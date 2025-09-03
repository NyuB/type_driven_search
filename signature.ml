module Ctype = struct
  type t =
    | Atom of string
    | Pointer of t

  let rec equal a b =
    match a, b with
    | Atom sa, Atom sb -> String.equal sa sb
    | Pointer pa, Pointer pb -> equal pa pb
    | _ -> false
  ;;

  let atom a = Atom a

  let parse param =
    let rec parse_stars current i =
      if i >= String.length param
      then current
      else if String.get param i == '*'
      then parse_stars (Pointer current) (i + 1)
      else parse_stars current (i + 1)
    in
    let rec aux current i =
      if i >= String.length param
      then Atom current
      else (
        let c = String.get param i in
        match c with
        | 'a' .. 'z' | 'A' .. 'Z' -> aux (String.cat current (Char.escaped c)) (i + 1)
        | _ -> parse_stars (Atom current) i)
    in
    aux "" 0
  ;;

  let rec string_of_t = function
    | Atom s -> s
    | Pointer p ->
      let ps = string_of_t p in
      String.cat ps "*"
  ;;
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

type cfunction =
  { name : string
  ; signature : t
  }

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

let parse str : t option =
  match String.split_on_char '(' str with
  | return :: params ->
    let params = trim_last_paren (String.concat "(" params) in
    Some
      { return = Atom (String.trim return)
      ; params =
          String.split_on_char ',' params
          |> List.map String.trim
          |> remove_empty
          |> List.map Ctype.parse
      }
  | [] -> None
;;
