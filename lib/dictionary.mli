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

val get_words : t -> Word.t list
val get_answers : t -> Word.t list
(* val filter_dictionary : t -> Information.t -> t
   val num_answers_remaining : t -> string -> string -> int *)
