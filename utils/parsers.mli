(** Generic parser combinators *)

(** A string and the current parsing position (i.e. first character to parser) within this string *)
type input = string * int

(** A parser ['a t] takes a string [s] and position [p] and return either 
- [None] if it cannot match from [p] or
- [Some ((s, p'), v] where [v] is the value parsed of type ['a] and [p'] is the next character to parse in [s]  *)
type 'a t = input -> (input * 'a) option
(** *)

val map : ('a -> 'b) -> 'a t -> 'b t
val flat_map : ('a -> 'b t) -> 'a t -> 'b t

(** [keyword kw] matches exactly [kw] *)
val keyword : string -> string t

(** Accepts alphanumeric identifiers, starting with a letter or underscore and containing letters, numbers or underscores *)
val identifier : string t

(** [zero_or_more t] parses zero or more contiguous items parsed by [t]

NB: this parser always succeeds and thus can return the input non-incremented, beware of infinite loops
*)
val zero_or_more : 'a t -> 'a list t

(** [first_of parsers] matches as the first successfull parser among [parsers], or does not match if no such parser exists *)
val first_of : 'a t list -> 'a t

(** [longest_of parsers] matches as the parser among [parsers] that matches the longest subsequence of the input, or does not match if no such parser exists *)
val longest_of : 'a t list -> 'a t

(** [option t] matches zero or one occurence of [t]

NB: this parser always succeeds and thus can return the input non-incremented, beware of infinite loops
*)
val option : 'a t -> 'a option t

(** [success value t] matches successfully zero character as [value]

NB: this parser always succeeds and thus can return the input non-incremented, beware of infinite loops
*)
val success : 'a -> 'b t -> 'a t

val discard : 'a t -> unit t
val skip : 'b t -> 'a t -> 'a t
val take : 'a t -> 'b t -> 'a t
val combine : 'b t -> 'a t -> ('a * 'b) t
val combine2 : 'c t -> ('a * 'b) t -> ('a * 'b * 'c) t
val combine3 : 'd t -> ('a * 'b * 'c) t -> ('a * 'b * 'c * 'd) t
val ( ||> ) : 'a t -> 'b t -> 'b t
val ( |. ) : 'a t -> 'b t -> 'a t
val ( |/ ) : 'a t -> ('a -> 'b) -> 'b t
val ( |>> ) : 'a t -> ('a -> 'b t) -> 'b t
val ( |* ) : 'a t -> 'b t -> ('a * 'b) t
val ( |** ) : ('a * 'b) t -> 'c t -> ('a * 'b * 'c) t
val ( |*** ) : ('a * 'b * 'c) t -> 'd t -> ('a * 'b * 'c * 'd) t
val list : prefix:'b t -> suffix:'c t -> sep:'d t -> 'a t -> 'a list t
val take_while : (char -> bool) -> string t

(** Matches zero or more whitespace characters among \s \t and \n 
NB: this does NOT include Windows CRLF
*)
val whitespaces : string t

(** [parse_full t string] returns [Some value] produced by [t] if it matched the whole [string], [None] otherwise *)
val parse_full : 'a t -> string -> 'a option
