open! Core

type t = { words : string list; answers : string list } [@@deriving sexp]

let create guesses answers ?num_guesses ?num_answers ?(shuffle = false) () =
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

  print_s [%sexp (guesses : string list)];
  print_s [%sexp (answers : string list)];
  { words = guesses @ answers; answers }

let get_words t = t.words
let get_answers t = t.answers

let filter_dictionary t information =
  let words =
    List.filter t.words ~f:(fun word ->
        not (Information.is_guess_useless information word))
  in
  let answers =
    List.filter t.answers ~f:(Information.can_word_be_answer information)
  in
  { words; answers }

(* TODO: add cache *)
let num_answers_remaining t information =
  List.count t.answers ~f:(Information.can_word_be_answer information)
