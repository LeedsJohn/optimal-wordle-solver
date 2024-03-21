open! Core

(* the string returned represents the information you would be give by Wordle.
   g -> green
   y -> yellow
   x -> gray *)
val evaluate : Word.t -> Word.t -> string
