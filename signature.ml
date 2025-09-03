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

let parse str : t option =
  match String.split_on_char '(' str with
  | return :: _ -> Some { return = String.trim return; params = [] }
  | [] -> None
;;
