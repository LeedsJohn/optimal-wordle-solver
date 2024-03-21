open! Core

type t = { word : string; id : int; possible_answer : bool } [@@deriving sexp]

let compare t1 t2 = Int.compare t1.id t2.id
let equal t1 t2 = Int.equal t1.id t2.id
let hash t = t.id
let hash_fold_t state t = Int.hash_fold_t state t.id
let words = Hashtbl.create (module String)
let count = ref 0

let create word ~possible_answer =
  Hashtbl.update_and_return words word ~f:(fun w ->
      match w with
      | Some w -> w
      | None ->
          let w = { word; id = !count; possible_answer } in
          count := !count + 1;
          w)

let empty_word = create "" ~possible_answer:false
let of_string word = Hashtbl.find_exn words word
let max_id = 15000
