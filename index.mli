module CFunction : sig
  (** A C/C++ function description *)
  type t =
    { name : string
    ; signature : Signature.t
    }

  val name : t -> string
  val signature : t -> Signature.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val string_of_t : t -> string
end

module type S = sig
  type t
  type config

  (** A descriptive name for this indexing method *)
  val id : string

  val init : config -> t

  (** [store index cfunctions] persists [cfunctions] into the [index] *)
  val store : t -> CFunction.t list -> unit

  (** [get index signature] retrieves all functions in [index] matching [signature] (ignoring parameters order) *)
  val get : t -> Signature.t -> CFunction.t list
end

(** For tests purposes, stores c-functions in non-persistent memory *)
module InMemory : S with type config = unit

(** Behaviour when initializing an index referring to a file *)
type config_open_mode =
  | Create (** Create a new index, possibly clearing the file *)
  | Connect (** Only read from the already persisted index in the file *)

type config_open_file =
  { file : string
  ; mode : config_open_mode
  }

(** Stores c-functions in a file, one by line *)
module FileBased : S with type config = config_open_file

(** Stores c-functions in a file, sorting them to improve query performance at the cost of storage performance *)
module FileBasedSorted : S with type config = config_open_file
