open! Core

(* t represents the information you would be given by Wordle.
   g -> green
   y -> yellow
   x -> gray *)
type t [@@deriving compare, equal, hash, sexp]

val of_string : string -> t
val to_string : t -> string
val get : t -> pos:int -> char
val evaluate : Word.t -> Word.t -> t
