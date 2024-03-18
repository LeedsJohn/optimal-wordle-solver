open! Core

let cache = Hashtbl.create (module Information)
let delta = 0.00001
let float_eq f1 f2 = Float.(abs (f1 - f2) <= delta)
let float_lt f1 f2 = Float.(f2 - f1 >= delta)
let float_gt f1 f2 = Float.(f1 - f2 >= delta)

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

let expected_answers_remaining dictionary guess =
  let remaining_answers = Dictionary.get_answers dictionary in
  let num_results = List.length remaining_answers |> Float.of_int in
  let possible_results = Hashtbl.create (module String) in
  List.iter remaining_answers ~f:(fun answer ->
      let result = Evaluator.evaluate guess answer in
      Hashtbl.update possible_results result ~f:(fun n ->
          match n with None -> 1 | Some n -> n + 1));
  Hashtbl.fold possible_results ~init:0. ~f:(fun ~key:_ ~data:count acc ->
      let count = Float.of_int count in
      Float.(acc + (count / num_results * count)))

let get_guesses dictionary n =
  let answers = Dictionary.get_answers dictionary in
  if List.length answers <= 2 then answers
  else
    let guesses =
      List.map (Dictionary.get_words dictionary) ~f:(fun word ->
          (word, expected_answers_remaining dictionary word))
      |> List.sort ~compare:(fun (_, n1) (_, n2) -> Float.compare n1 n2)
    in
    List.map (List.take guesses n) ~f:fst

let cache_hits = ref 0

let rec get_guess ~dictionary ~information ~max_guesses ~exploration_rate =
  let eval_guess dictionary guess best_so_far =
    let remaining_answers = Dictionary.get_answers dictionary in
    let counts = get_counts remaining_answers information guess in
    let num_answers = List.length remaining_answers in
    let total_guesses, _ =
      Hashtbl.fold counts ~init:(Float.zero, Float.zero)
        ~f:(fun
            ~key:information ~data:count (total_guesses, answers_examined) ->
          let open Float in
          let best_possible_ev =
            get_best_possible_ev total_guesses answers_examined num_answers
          in
          if float_gt best_possible_ev best_so_far then (infinity, infinity)
          else
            let _, expected_num_guesses =
              get_guess ~dictionary ~information
                ~max_guesses:Int.(max_guesses - 1)
                ~exploration_rate
            in
            let c = of_int count in
            ( total_guesses + ((1. + expected_num_guesses) * c),
              answers_examined + c ))
    in
    Float.(total_guesses / of_int num_answers)
  in
  if Information.all_greens information then ("", 0.)
  else if max_guesses = 0 then ("", Float.infinity)
  else
    match Hashtbl.find cache information with
    | Some res ->
        cache_hits := !cache_hits + 1;
        res
    | None ->
        let dictionary = Dictionary.filter_dictionary dictionary information in
        let guesses = get_guesses dictionary exploration_rate in
        let res =
          List.fold guesses ~init:("", Float.infinity) ~f:(fun acc word ->
              let _best_word, best_score = acc in
              let score = eval_guess dictionary word best_score in
              if float_lt score best_score then (word, score) else acc)
        in
        Hashtbl.add_exn cache ~key:information ~data:res;
        res

let cache_size () = Hashtbl.length cache
let num_cache_hits () = !cache_hits

let rec play_game_aux ~answer ~path ~dictionary ~information ~max_guesses
    ~exploration_rate =
  if List.length path > 0 && String.(List.hd_exn path = answer) then
    List.rev path
  else
    let guess, _expected_score =
      get_guess ~dictionary ~information ~max_guesses ~exploration_rate
    in
    let information = Information.add_information information ~guess ~answer in
    let max_guesses = max_guesses - 1 in
    play_game_aux ~answer ~path:(guess :: path) ~dictionary ~information
      ~max_guesses ~exploration_rate

let play_game answer =
  let information =
    Information.add_information Information.empty ~guess:"salet" ~answer
  in
  let dictionary = Dictionary.create "guesses.txt" "answers.txt" () in
  let dictionary = Dictionary.filter_dictionary dictionary information in
  play_game_aux ~answer ~path:[ "salet" ] ~dictionary ~information
    ~max_guesses:4 ~exploration_rate:20
