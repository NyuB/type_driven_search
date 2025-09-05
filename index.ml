module CFunction = struct
  type t =
    { name : string
    ; signature : Signature.t
    }

  let equal { name = na; signature = sa } { name = nb; signature = sb } =
    String.equal na nb && Signature.equal sa sb
  ;;

  let string_of_t { name; signature } =
    Printf.sprintf
      "%s %s(%s)"
      name
      (Signature.Ctype.string_of_t signature.return)
      (String.concat ", " (List.map Signature.Ctype.string_of_t signature.params))
  ;;

  let signature { name = _; signature } = signature
end

module type S = sig
  type t
  type config

  val init : config -> t
  val store : t -> CFunction.t list -> unit
  val get : t -> Signature.t -> CFunction.t list
end

module InMemory : S with type config = unit = struct
  type t = CFunction.t list ref
  type config = unit

  let init () = ref []
  let store t list = t := List.append !t list

  let get t s : CFunction.t list =
    List.filter (Fun.compose (Signature.equal s) CFunction.signature) !t
  ;;
end
