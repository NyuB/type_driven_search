module Ctype : sig
  type t =
    | Atom of string
    | Const of t
    | Pointer of t
    | Var_Args

  val compare : t -> t -> int
  val equal : ('a -> 'b -> int) -> 'a -> 'b -> bool
  val atom : string -> t
  val pointer : t -> t
  val const : t -> t
  val parser : t Parsers.t
  val parse : string -> t
  val string_of_t : t -> string
  val explain : t -> string
end

type t =
  { return : Ctype.t
  ; params : Ctype.t list
  }

val return : t -> Ctype.t
val params : t -> Ctype.t list
val string_of_t : t -> string
val compare : t -> t -> int
val equal : t -> t -> bool
val canonical : t -> t
val parser : t Parsers.t
val parse : string -> t option
val explain : t -> string
