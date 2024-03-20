open! Core

val get_guess :
  guesses:string list ->
  answers:string list ->
  max_guesses:int ->
  exploration_rate:int ->
  prev_results:(string * string) list ->
  string * float

val play_game : answer:string -> exploration_rate:int -> string list
val play_game_interactive : unit -> unit
val get_total_guesses : string list -> int -> int

val create_cache :
  guesses:string list -> answers:string list -> exploration_rate:int -> unit
