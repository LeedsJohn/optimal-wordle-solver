open! Core

type t = { word : string; id : int; possible_answer : bool }
[@@deriving compare, equal, hash, sexp]

val create : string -> possible_answer:bool -> t
val of_string : string -> t
val empty_word : t
val max_id : int
(* IMPORTANT: this is manually set to 15,000 and must be updated if a wordlist exceeds 15,000 words *)
