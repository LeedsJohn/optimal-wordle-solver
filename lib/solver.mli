open! Core

val get_guess :
  guesses:Word.t list ->
  answers:Word.t list ->
  max_guesses:int ->
  exploration_rate:int ->
  prev_results:(Word.t * Evaluation.t) list ->
  Word.t * float

val play_game : answer:string -> exploration_rate:int -> Word.t list
val play_game_interactive : unit -> unit
val get_total_guesses : Word.t list -> int -> int

val create_cache :
  guesses:Word.t list -> answers:Word.t list -> exploration_rate:int -> unit
