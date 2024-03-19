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
      let new_info =
        Information.add_information information ~guess
          ~result:(Evaluator.evaluate guess answer)
      in
      Hashtbl.update counts new_info ~f:(fun n ->
          match n with None -> 1 | Some n -> n + 1));
  counts

let expected_answers_remaining answers guess =
  let num_results = List.length answers |> Float.of_int in
  let possible_results = Hashtbl.create (module String) in
  List.iter answers ~f:(fun answer ->
      let result = Evaluator.evaluate guess answer in
      Hashtbl.update possible_results result ~f:(fun n ->
          match n with None -> 1 | Some n -> n + 1));
  Hashtbl.fold possible_results ~init:0. ~f:(fun ~key:_ ~data:count acc ->
      let count = Float.of_int count in
      Float.(acc + (count / num_results * count)))

let get_guesses words answers n =
  if List.length answers = 1 && String.(List.hd_exn answers = "boozy") then
    print_endline "ok here we are";
  if List.length answers <= 2 then (
    printf "yep the length is under 2 - returning answers: ";
    print_s [%sexp (answers : string list)];
    answers)
  else
    let guesses =
      List.map words ~f:(fun word ->
          (word, expected_answers_remaining answers word))
      |> List.sort ~compare:(fun (_, n1) (_, n2) -> Float.compare n1 n2)
    in
    List.map (List.take guesses n) ~f:fst

let cache_hits = ref 0

let rec get_guess ~guesses ~answers ~information ~max_guesses ~exploration_rate
    =
  let eval_guess answers guess best_so_far =
    let counts = get_counts answers information guess in
    let num_answers = List.length answers in
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
              get_guess ~guesses ~answers ~information
                ~max_guesses:Int.(max_guesses - 1)
                ~exploration_rate
            in
            let c = of_int count in
            ( total_guesses + ((1. + expected_num_guesses) * c),
              answers_examined + c ))
    in
    Float.(total_guesses / of_int num_answers)
  in
  if List.length answers = 1 then (List.hd_exn answers, 1.)
  else if max_guesses = 0 then ("", Float.infinity)
  else
    match Hashtbl.find cache information with
    | Some res ->
        cache_hits := !cache_hits + 1;
        res
    | None ->
        let answers =
          List.filter answers ~f:(Information.can_word_be_answer information)
        in
        let guesses = get_guesses guesses answers exploration_rate in
        let res =
          List.fold guesses ~init:("", Float.infinity) ~f:(fun acc word ->
              let _best_word, best_score = acc in
              let score = eval_guess answers word best_score in
              if float_lt score best_score then (word, score) else acc)
        in
        if max_guesses > 2 then Hashtbl.add_exn cache ~key:information ~data:res;
        res

let cache_size () = Hashtbl.length cache
let num_cache_hits () = !cache_hits

let rec play_game_aux ~answer ~path ~dictionary ~information ~max_guesses
    ~exploration_rate =
  if String.(List.hd_exn path = answer) then List.rev path
  else
    let guess, _expected_score =
      get_guess
        ~guesses:(Dictionary.get_words dictionary)
        ~answers:(Dictionary.get_answers dictionary)
        ~information ~max_guesses ~exploration_rate
    in
    let information =
      Information.add_information information ~guess
        ~result:(Evaluator.evaluate guess answer)
    in
    let max_guesses = max_guesses - 1 in
    play_game_aux ~answer ~path:(guess :: path) ~dictionary ~information
      ~max_guesses ~exploration_rate

let play_game answer =
  let guess = "salet" in
  let information =
    Information.add_information Information.empty ~guess
      ~result:(Evaluator.evaluate guess answer)
  in
  let path = [ "salet" ] in
  let dictionary = Dictionary.create "guesses.txt" "answers.txt" () in
  let dictionary = Dictionary.filter_dictionary dictionary information in
  printf "NUM ANSWERS: %d\n" (List.length (Dictionary.get_answers dictionary));
  print_s [%sexp (Dictionary.get_answers dictionary : string list)];
  Out_channel.flush Out_channel.stdout;
  play_game_aux ~answer ~path ~dictionary ~information ~max_guesses:4
    ~exploration_rate:20

let get_total_guesses possible_answers =
  let dictionary = Dictionary.create "guesses.txt" "answers.txt" () in
  List.foldi possible_answers ~init:0 ~f:(fun i acc answer ->
      let information =
        Information.add_information Information.empty ~guess:"salet"
          ~result:(Evaluator.evaluate "salet" answer)
      in
      let path =
        play_game_aux ~answer ~path:[ "salet" ] ~dictionary ~information
          ~max_guesses:4 ~exploration_rate:10
      in
      let acc = acc + List.length path in
      printf "%S (%d): %d Cache size: %d Cache hits: %d " answer i acc
        (cache_size ()) !cache_hits;
      print_s [%sexp (path : string list)];
      Out_channel.flush Out_channel.stdout;
      acc)

let rec play_game_interactive_aux guesses answers information max_guesses =
  printf "Enter your guess or press enter to get a recommendation: ";
  Out_channel.flush Out_channel.stdout;
  let guess = In_channel.input_line_exn In_channel.stdin in
  let guess =
    if String.length guess <> 0 then guess
    else
      let recommended_guess, ev =
        get_guess ~guesses ~answers ~information ~max_guesses
          ~exploration_rate:20
      in
      printf "Recommended guess: %s (expected guesses: %f): " recommended_guess
        ev;
      Out_channel.flush Out_channel.stdout;
      In_channel.input_line_exn In_channel.stdin
  in
  printf "Enter your result: ";
  Out_channel.flush Out_channel.stdout;
  let result = In_channel.input_line_exn In_channel.stdin in
  if String.(result = "ggggg") then print_endline "good job!"
  else
    let answers =
      List.filter answers ~f:(fun answer ->
          String.(Evaluator.evaluate guess answer = result))
    in
    let information = Information.add_information information ~guess ~result in
    play_game_interactive_aux guesses answers information (max_guesses - 1)

let play_game_interactive () =
  printf "Enter your guess: ";
  Out_channel.flush Out_channel.stdout;
  let guess = In_channel.input_line_exn In_channel.stdin in
  printf "Enter your result: ";
  Out_channel.flush Out_channel.stdout;
  let result = In_channel.input_line_exn In_channel.stdin in
  let information =
    Information.add_information Information.empty ~guess ~result
  in
  let dictionary = Dictionary.create "guesses.txt" "answers.txt" () in
  let guesses, answers =
    (Dictionary.get_words dictionary, Dictionary.get_answers dictionary)
  in
  play_game_interactive_aux guesses answers information 5
