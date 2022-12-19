type t

val empty : t

val add_word : string -> t -> t

val lookup_string_of_int : int -> t -> string option

val lookup_int_of_string : string -> t -> int option

val adjacent_words : int -> t -> Int_set.t
