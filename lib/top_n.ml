open! Core

type t = {
  ar : (float * Word.t) Array.t;
  mutable min_score : float;
  mutable min_word : Word.t;
  mutable max_pos : int;
  mutable max_score : float;
}

let empty_entry = (Float.infinity, Word.empty_word)

let create ~n =
  {
    ar = Array.create ~len:n empty_entry;
    min_score = Float.infinity;
    min_word = Word.empty_word;
    max_pos = 0;
    max_score = Float.infinity;
  }

let update_pos t =
  t.min_score <- Float.infinity;
  t.min_word <- Word.empty_word;
  t.max_score <- Float.neg_infinity;
  t.max_pos <- -1;
  Array.iteri t.ar ~f:(fun i (score, word) ->
      if Float.(score < t.min_score) then (
        t.min_score <- score;
        t.min_word <- word);
      if Float.(score > t.max_score) then (
        t.max_pos <- i;
        t.max_score <- score))

let add t ~word ~score =
  if Float.(score >= t.max_score) then ()
  else (
    t.ar.(t.max_pos) <- (score, word);
    update_pos t)

let get_min t = (t.min_score, t.min_word)

let to_list t =
  Array.fold t.ar ~init:[] ~f:(fun acc (_, word) ->
      if Word.equal word Word.empty_word then acc else word :: acc)
