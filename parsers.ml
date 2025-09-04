type input = string * int
type 'a t = input -> (input * 'a) option

let map (f : 'a -> 'b) (t : 'a t) : 'b t =
  fun input -> Option.map (fun (input, a) -> input, f a) (t input)
;;

let option_flat_map f opt = Option.bind opt f

let flat_map (f : 'a -> 'b t) (t : 'a t) : 'b t =
  fun input -> t input |> option_flat_map (fun (input, value) -> f value @@ input)
;;

let keyword kw : string t =
  let kwl = String.length kw in
  fun (s, i) ->
    let l = String.length s in
    if l - i < kwl
    then None
    else (
      let sub = String.sub s i kwl in
      if String.equal sub kw then Some ((s, i + kwl), kw) else None)
;;

let identifier_symbol = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false
;;

let identifier : string t =
  fun (s, i) ->
  let l = String.length s in
  let rec aux j =
    if j >= l || (not @@ identifier_symbol (String.get s j))
    then
      if j == i
      then None
      else (
        let res = String.sub s i (j - i) in
        Some ((s, j), res))
    else aux (j + 1)
  in
  aux i
;;

let zero_or_more (t : 'a t) : 'a list t =
  let rec aux acc input =
    match t input with
    | None -> Some (input, List.rev acc)
    | Some (input, value) -> aux (value :: acc) input
  in
  aux []
;;

let first_of (parsers : 'a t list) : 'a t =
  fun input ->
  List.fold_left
    (fun acc parser -> if Option.is_some acc then acc else parser input)
    None
    parsers
;;

let longest_of (parsers : 'a t list) : 'a t =
  fun input ->
  List.fold_left
    (fun acc parser ->
       match acc, parser input with
       | None, v | v, None -> v
       | (Some ((_, i), _) as vi), (Some ((_, j), _) as vj) -> if i >= j then vi else vj)
    None
    parsers
;;

let option (t : 'a t) : 'a option t =
  fun input ->
  match t input with
  | Some (input, value) -> Some (input, Some value)
  | None -> Some (input, None)
;;

let success v t = map (fun _ -> v) t
let discard t = map (fun _ -> ()) t
let skip (t : 'b t) (previous : 'a t) : 'a t = previous |> flat_map (fun v -> success v t)
let take parser previous = previous |> flat_map (fun _ -> parser)
let combine (b : 'b t) (a : 'a t) = a |> flat_map (fun va -> map (fun vb -> va, vb) b)

let combine2 (c : 'c t) (ab : ('a * 'b) t) =
  ab |> flat_map (fun (va, vb) -> map (fun vc -> va, vb, vc) c)
;;

let combine3 (d : 'd t) (abc : ('a * 'b * 'c) t) =
  abc |> flat_map (fun (va, vb, vc) -> map (fun vd -> va, vb, vc, vd) d)
;;

(* Syntax *)
include struct
  let ( ||> ) previous parser = take parser previous
  let ( |. ) previous parser = skip parser previous
  let ( |/ ) parser f = map f parser
  let ( |>> ) parser f = flat_map f parser
  let ( |* ) previous parser = combine parser previous
  let ( |** ) previous parser = combine2 parser previous
  let ( |*** ) previous parser = combine3 parser previous
end

let list ~prefix ~suffix ~sep (t : 'a t) : 'a list t =
  let rec aux acc input =
    match t input with
    | None -> Some (input, List.rev acc)
    | Some (input, value) ->
      (match sep input with
       | None -> aux (value :: acc) input
       | Some (input, _) -> aux (value :: acc) input)
  in
  discard prefix |> flat_map (fun () -> aux []) |> skip suffix
;;

let take_while pred : string t =
  fun (s, i) ->
  let l = String.length s in
  let rec aux j =
    if j >= l || (not @@ pred (String.get s j))
    then Some ((s, j), String.sub s i (j - i))
    else aux (j + 1)
  in
  aux i
;;

let whitespaces = take_while (( = ) ' ')
let parse t input = t input

let parse_full t input =
  match parse t (input, 0) with
  | Some ((s, i), value) -> if String.length s == i then Some value else None
  | None -> None
;;
