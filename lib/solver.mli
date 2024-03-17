open! Core

val get_guess : Dictionary.t -> Information.t -> string * float
val cache_size : unit -> int
val num_cache_hits : unit -> int
