type t

val empty : t

val add_edge : int -> int -> t -> t

val find_path : int -> int -> t -> int list option
