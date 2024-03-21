open! Core

(* This is a data structure to track the n smallest items in a list.
   At the moment, add is an O(n) operation which is inefficient, but I think the easiest
   way to get reduce the time complexity to O(log(n)) would be to use a priority queue,
   but afaik there's no built-in priority queue in Core or the OCaml standard library. *)

type t

val create : n:int -> t
val add : t -> word:Word.t -> score:float -> unit
val get_min : t -> float * Word.t
val to_list : t -> Word.t list
