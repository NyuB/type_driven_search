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

module CFunction : sig
  type s = t

  (** A C/C++ function description *)
  type t =
    { name : string
    ; signature : s
    }

  val name : t -> string
  val signature : t -> s
  val compare : t -> t -> int
  val equal : t -> t -> bool

  (** Matches C-style function declarations, e.g.
  {[ int add(int, int); ]} *)
  val parser : t Parsers.t

  (** see {!val:CFunction.parser} *)
  val parse : string -> t option

  val string_of_t : t -> string
end
