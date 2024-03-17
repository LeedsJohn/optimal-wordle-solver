open! Core

type t = { words : string list; answers : string list }

let create guesses answers ?num_guesses ?num_answers () =
  let crop l n = match n with None -> l | Some n -> List.take l n in
  let guesses = crop (In_channel.read_lines guesses) num_guesses in
  let answers = crop (In_channel.read_lines answers) num_answers in
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
    List.filter t.answers ~f:(fun word ->
        Information.can_word_be_answer information word)
  in
  { words; answers }
