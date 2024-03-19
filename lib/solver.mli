open! Core

val get_guess :
  guesses:string list ->
  answers:string list ->
  information:Information.t ->
  first_guess:bool ->
  max_guesses:int ->
  exploration_rate:int ->
  string * float

val play_game : string -> string list
val play_game_interactive : unit -> unit
val get_total_guesses : string list -> string -> int -> int
val create_cache : string list -> string list -> unit
