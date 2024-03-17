open! Core

let cache = Hashtbl.create (module Information)
let alpha = 0.00001
let float_eq f1 f2 = Float.(abs (f1 - f2) <= alpha)
let float_lt f1 f2 = Float.(f2 - f1 >= alpha)
let float_gt f1 f2 = Float.(f1 - f2 >= alpha)

[@@@ocaml.warning "-32"]

let count_letters answers =
  let counts = Array.create ~len:26 0 in
  List.iter answers ~f:(fun word ->
      String.iter word ~f:(fun c ->
          let n = Char.to_int c - Char.to_int 'a' in
          counts.(n) <- counts.(n) + 1));
  counts

[@@@ocaml.warning "-32"]

let score_word counts information word =
  let score_char c =
    let remaining_characters = counts.(Char.to_int c - Char.to_int 'a') in
    if Information.char_guessed information c then remaining_characters / 4
    else remaining_characters
  in
  String.fold word ~init:0 ~f:(fun acc c -> acc + score_char c)

let get_best_possible_ev guesses_taken answers_examined num_answers =
  let open Float in
  let num_answers = of_int num_answers in
  let remaining_answers = num_answers - answers_examined in
  let remaining_best = ((2. * remaining_answers) - 1.) / remaining_answers in
  let current =
    if float_eq zero answers_examined then 0.
    else guesses_taken / answers_examined
  in
  (remaining_best * (remaining_answers / num_answers))
  + (current * (answers_examined / num_answers))

let get_counts remaining_answers information guess =
  let counts = Hashtbl.create (module Information) in
  List.iter remaining_answers ~f:(fun answer ->
      let new_info = Information.add_information information ~guess ~answer in
      Hashtbl.update counts new_info ~f:(fun n ->
          match n with None -> 1 | Some n -> n + 1));
  counts

let cache_hits = ref 0
let stuff = ref []

let show_stuff () =
  print_endline "Showing stuff: ";
  printf "cur length: %d\n" (List.length !stuff);
  List.iter (List.rev !stuff) ~f:(fun (guess, info) ->
      printf "\n\nGuess: %S\n" guess;
      Information.show info);
  print_endline "end show stuff"

let rec get_guess dictionary information =
  let eval_guess guess remaining_answers best_so_far =
    let counts = get_counts remaining_answers information guess in
    let num_answers = List.length remaining_answers in
    let total_guesses, _ =
      Hashtbl.fold counts ~init:(Float.zero, Float.zero)
        ~f:(fun ~key:info ~data:count (total_guesses, answers_examined) ->
          let open Float in
          let best_possible_ev =
            get_best_possible_ev total_guesses answers_examined num_answers
          in
          if float_gt best_possible_ev best_so_far then (infinity, infinity)
          else (
            stuff := (guess, info) :: !stuff;
            let _, expected_num_guesses = get_guess dictionary info in
            stuff := List.tl_exn !stuff;
            let c = of_int count in
            ( total_guesses + ((1. + expected_num_guesses) * c),
              answers_examined + c )))
    in
    Float.(total_guesses / of_int num_answers)
  in
  if Information.all_greens information then ("", 0.)
  else
    match Hashtbl.find cache information with
    | Some res ->
        cache_hits := !cache_hits + 1;
        res
    | None ->
        let dictionary = Dictionary.filter_dictionary dictionary information in
        let remaining_answers = Dictionary.get_answers dictionary in
        let counts = count_letters remaining_answers in
        let guesses =
          List.sort (Dictionary.get_words dictionary) ~compare:(fun w1 w2 ->
              score_word counts information w2
              - score_word counts information w1)
        in
        let res =
          List.fold guesses ~init:("", Float.infinity) ~f:(fun acc word ->
              let _best_word, best_score = acc in
              let score = eval_guess word remaining_answers best_score in
              if float_lt score best_score then (word, score) else acc)
        in
        Hashtbl.add_exn cache ~key:information ~data:res;
        res

let cache_size () = Hashtbl.length cache
let num_cache_hits () = !cache_hits
