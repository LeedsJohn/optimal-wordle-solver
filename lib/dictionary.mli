open! Core

type t

val create :
  string -> string -> ?num_guesses:int -> ?num_answers:int -> unit -> t

val get_words : t -> string list
val get_answers : t -> string list
val filter_dictionary : t -> Information.t -> t
