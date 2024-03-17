open! Core

type t [@@deriving sexp]

val create :
  string ->
  string ->
  ?num_guesses:int ->
  ?num_answers:int ->
  ?shuffle:bool ->
  unit ->
  t

val get_words : t -> string list
val get_answers : t -> string list
val filter_dictionary : t -> Information.t -> t
