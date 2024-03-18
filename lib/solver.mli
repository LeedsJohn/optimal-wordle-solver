open! Core

val get_guess :
  dictionary:Dictionary.t ->
  information:Information.t ->
  max_guesses:int ->
  exploration_rate:int ->
  string * float

val play_game : string -> string list
val cache_size : unit -> int
val num_cache_hits : unit -> int
