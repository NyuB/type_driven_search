type ctype = string

type t =
  { return : ctype
  ; params : ctype list
  }

let string_of_t t = Printf.sprintf "%s (%s)" t.return (String.concat "," t.params)

let equal a b =
  String.equal a.return b.return && List.equal String.equal a.params b.params
;;

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
      { return = String.trim return
      ; params = String.split_on_char ',' params |> List.map String.trim |> remove_empty
      }
  | [] -> None
;;
