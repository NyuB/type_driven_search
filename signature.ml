module Parser = struct
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

  let skip (t : 'b t) (previous : 'a t) : 'a t =
    previous |> flat_map (fun v -> success v t)
  ;;

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
end

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
    let open Parser in
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
    let open Parser in
    keyword "unsigned "
    |. whitespaces
    |* first_of (List.map keyword [ "char"; "int"; "long"; "short" ])
    |/ fun (u, t) -> String.cat u t
  ;;

  let parser : t Parser.t =
    let open Parser in
    discard whitespaces
    ||> longest_of [ identifier; unsigned ]
    |. whitespaces
    |* zero_or_more qualifier_parser
    |/ fun (id, stars) -> qualify (Atom id) stars
  ;;

  let parse param =
    Parser.parse_full parser param
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

let parser =
  let open Parser in
  let sep = discard whitespaces |>> (fun () -> keyword ",") |. whitespaces in
  let prefix = discard whitespaces |>> (fun () -> keyword "(") |. whitespaces
  and suffix = discard whitespaces |>> (fun () -> keyword ")") |. whitespaces in
  discard whitespaces
  ||> Ctype.parser
  |* list ~prefix ~suffix ~sep Ctype.parser
  |/ fun (return, params) -> { return; params }
;;

let parse str : t option = Parser.parse_full parser str

let explain t =
  Printf.sprintf
    "a function returning %s from (%s)"
    (Ctype.explain t.return)
    (String.concat ", " (List.map Ctype.explain t.params))
;;
