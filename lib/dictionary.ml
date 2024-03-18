open! Core

type t = { words : string list; answers : string list } [@@deriving sexp]

let create guesses answers ?num_guesses ?num_answers ?(shuffle = true) () =
  let crop l n = match n with None -> l | Some n -> List.take l n in
  let guesses = In_channel.read_lines guesses in
  let answers = In_channel.read_lines answers in
  let guesses, answers =
    if shuffle then
      let random_state = Random.State.make [| 0 |] in
      (List.permute ~random_state guesses, List.permute ~random_state answers)
    else (guesses, answers)
  in
  let guesses, answers = (crop guesses num_guesses, crop answers num_answers) in
  { words = guesses @ answers; answers }

let get_words t = t.words
let get_answers t = t.answers

let is_good_answer guess result answer =
  String.(Evaluator.evaluate guess answer = result)

let filter_dictionary t information =
  let answers =
    List.filter t.answers ~f:(Information.can_word_be_answer information)
  in
  { t with answers }

let num_answers_remaining t guess result =
  List.count t.answers ~f:(is_good_answer guess result)
