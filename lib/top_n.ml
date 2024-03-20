open! Core

type t = {
  ar : (float * string) Array.t;
  used_words : string Hash_set.t;
  mutable min_score : float;
  mutable min_word : string;
  mutable max_pos : int;
  mutable max_score : float;
  mutable max_word : string;
}

let empty_entry = (Float.infinity, "")

let create len =
  {
    ar = Array.create ~len empty_entry;
    used_words = Hash_set.create (module String);
    min_score = Float.infinity;
    min_word = "";
    max_pos = 0;
    max_score = Float.infinity;
    max_word = "";
  }

let update_pos t =
  t.min_score <- Float.infinity;
  t.min_word <- "";
  t.max_score <- Float.neg_infinity;
  t.max_word <- "";
  t.max_pos <- -1;
  Array.iteri t.ar ~f:(fun i (score, word) ->
      if Float.(score < t.min_score) then (
        t.min_score <- score;
        t.min_word <- word);
      if Float.(score > t.max_score) then (
        t.max_pos <- i;
        t.max_score <- score;
        t.max_word <- word))

let add t word score =
  if Float.(score >= t.max_score) || Hash_set.mem t.used_words word then ()
  else (
    Hash_set.remove t.used_words t.max_word;
    Hash_set.add t.used_words word;
    t.ar.(t.max_pos) <- (score, word);
    update_pos t)

let get_min t = (t.min_score, t.min_word)

let to_list t =
  Array.fold t.ar ~init:[] ~f:(fun acc (_, word) ->
      if String.(word = "") then acc else word :: acc)
