open! Core

type t [@@deriving compare, equal, hash, sexp]

val create : string -> possible_answer:bool -> t
val of_string : string -> t
val empty_word : t
val get_char : t -> int -> char
val to_string : t -> string
val count_occurrences : t -> char -> int
val possible_answer : t -> bool
