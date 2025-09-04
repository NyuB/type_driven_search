type input = string * int
type 'a t = input -> (input * 'a) option

val map : ('a -> 'b) -> 'a t -> 'b t
val flat_map : ('a -> 'b t) -> 'a t -> 'b t
val keyword : string -> string t
val identifier : string t
val zero_or_more : 'a t -> 'a list t
val first_of : 'a t list -> 'a t
val longest_of : 'a t list -> 'a t
val option : 'a t -> 'a option t
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
val whitespaces : string t
val parse_full : ('a * int -> ((string * int) * 'b) option) -> 'a -> 'b option
