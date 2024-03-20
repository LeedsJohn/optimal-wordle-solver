open! Core

type t

val create : int -> t
val add : t -> string -> float -> unit
val get_min : t -> float * string
val to_list : t -> string list
