open! Core

type t [@@deriving compare, hash, sexp]

val empty : t
val can_word_be_answer : t -> string -> bool
val add_information : t -> guess:string -> result:string -> t
val all_greens : t -> bool
val is_guess_useless : t -> string -> bool
val char_guessed : t -> char -> bool
val show : t -> unit
