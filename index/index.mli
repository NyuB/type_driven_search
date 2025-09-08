module type S = sig
  type t
  type config

  (** A descriptive name for this indexing method *)
  val id : string

  val init : config -> t

  (** [store index cfunctions] persists [cfunctions] into the [index] *)
  val store : t -> Signature.CFunction.t list -> unit

  (** [get index signature] retrieves all functions in [index] with the given [signature] (ignoring parameters order) *)
  val get : t -> Signature.t -> Signature.CFunction.t list

  (** [query index signature] retrieves all functions in index matching [signature]; A given signature [candidate] is considered matching [signature] if it has the same return type and [signature]'s parameters are a subset of [candidate]'s parameters *)
  val query : t -> Signature.t -> Signature.CFunction.t list
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
