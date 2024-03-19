open! Core

module Guess_ev = struct
  type t = string * float [@@deriving sexp]
end

let cache =
  if Sys_unix.file_exists_exn "cache.sexp" then
    Sexp.load_sexp "cache.sexp"
    |> Hashtbl.m__t_of_sexp (module Information) Guess_ev.t_of_sexp
  else Hashtbl.create (module Information)

let delta = 0.00001
let float_eq f1 f2 = Float.(abs (f1 - f2) <= delta)

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
          match n with None -> (answer, 1) | Some (a, n) -> (a, n + 1)));
  counts

let expected_answers_remaining answers guess =
  let num_results = List.length answers |> Float.of_int in
  let possible_results = Hashtbl.create (module String) in
  List.iter answers ~f:(fun answer ->
      if String.(guess = answer) then ()
      else
        let result = Evaluator.evaluate guess answer in
        Hashtbl.update possible_results result ~f:(fun n ->
            match n with None -> 1 | Some n -> n + 1));
  Hashtbl.fold possible_results ~init:0. ~f:(fun ~key:_ ~data:count acc ->
      let count = Float.of_int count in
      Float.(acc + (count * count / num_results)))

let get_guesses words answers n =
  (* if there are <= 3 answers, we always want to choose one of those to be our guess *)
  if List.length answers <= 3 then answers
  else
    let guesses =
      List.map words ~f:(fun word ->
          (word, expected_answers_remaining answers word))
      |> List.sort ~compare:(fun (_, n1) (_, n2) -> Float.compare n1 n2)
    in
    List.map (List.take guesses n) ~f:fst

let rec get_guess ~guesses ~answers ~information ~first_guess ~max_guesses
    ~exploration_rate =
  let eval_guess answers guess best_so_far =
    let counts = get_counts answers information guess in
    let num_answers = List.length answers in
    let total_guesses, _ =
      Hashtbl.fold counts ~init:(Float.zero, Float.zero)
        ~f:(fun
            ~key:information
            ~data:(answer, count)
            (total_guesses, answers_examined)
          ->
          let open Float in
          let best_possible_ev =
            get_best_possible_ev total_guesses answers_examined num_answers
          in
          if best_possible_ev >= best_so_far then (infinity, infinity)
          else
            let res = Evaluator.evaluate guess answer in
            let new_answers =
              List.filter answers ~f:(fun a ->
                  String.(Evaluator.evaluate guess a = res))
            in
            let _, expected_num_guesses =
              get_guess ~guesses ~answers:new_answers ~information
                ~first_guess:false
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
  else if first_guess && Hashtbl.mem cache information then
    Hashtbl.find_exn cache information
  else
    (* let answers =
         List.filter answers ~f:(Information.can_word_be_answer information)
       in *)
    let guesses = get_guesses guesses answers exploration_rate in
    let res =
      List.fold guesses ~init:("", Float.infinity) ~f:(fun acc word ->
          let _best_word, best_score = acc in
          let score = eval_guess answers word best_score in
          if Float.(score < best_score) then (word, score) else acc)
    in
    res

let rec play_game_aux ~answer ~path ~guesses ~answers ~information ~max_guesses
    ~exploration_rate =
  if String.(List.hd_exn path = answer) then List.rev path
  else
    let guess, _expected_score =
      get_guess ~guesses ~answers ~information ~first_guess:true ~max_guesses
        ~exploration_rate
    in
    let information =
      Information.add_information information ~guess
        ~result:(Evaluator.evaluate guess answer)
    in
    let max_guesses = max_guesses - 1 in
    let res = Evaluator.evaluate guess answer in
    let answers =
      List.filter answers ~f:(fun a ->
          String.(Evaluator.evaluate guess a = res))
    in
    play_game_aux ~answer ~path:(guess :: path) ~guesses ~answers ~information
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
  let guesses, answers =
    (Dictionary.get_words dictionary, Dictionary.get_answers dictionary)
  in
  printf "NUM ANSWERS: %d\n" (List.length (Dictionary.get_answers dictionary));
  print_s [%sexp (Dictionary.get_answers dictionary : string list)];
  Out_channel.flush Out_channel.stdout;
  play_game_aux ~answer ~path ~guesses ~answers ~information ~max_guesses:4
    ~exploration_rate:40

let get_total_guesses possible_answers starting_word exploration_rate =
  let dictionary = Dictionary.create "guesses.txt" "answers.txt" () in
  let guesses = Dictionary.get_words dictionary in
  let all_answers = Dictionary.get_answers dictionary in
  List.foldi possible_answers ~init:0 ~f:(fun _i acc answer ->
      let information =
        Information.add_information Information.empty ~guess:starting_word
          ~result:(Evaluator.evaluate starting_word answer)
      in
      let res = Evaluator.evaluate "salet" answer in
      let answers =
        List.filter all_answers ~f:(fun a ->
            String.(Evaluator.evaluate "salet" a = res))
      in
      Out_channel.flush Out_channel.stdout;
      let path =
        play_game_aux ~answer ~path:[ "salet" ] ~guesses ~answers ~information
          ~max_guesses:4 ~exploration_rate
      in
      let acc = acc + List.length path in
      printf "%s: " answer;
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
        get_guess ~guesses ~answers ~information ~first_guess:true ~max_guesses
          ~exploration_rate:40
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

let create_cache guesses answers =
  let guess = "salet" in
  List.iteri answers ~f:(fun i answer ->
      printf "%d\n" i;
      Out_channel.flush Out_channel.stdout;
      let result = Evaluator.evaluate guess answer in
      let information =
        Information.add_information Information.empty ~guess ~result
      in
      Hashtbl.update cache information ~f:(fun res ->
          match res with
          | Some res -> res
          | None ->
              let answers =
                List.filter answers ~f:(fun answer ->
                    String.(Evaluator.evaluate "salet" answer = result))
              in
              get_guess ~guesses ~answers ~information ~first_guess:false
                ~max_guesses:5 ~exploration_rate:40));
  let s = Hashtbl.sexp_of_t Information.sexp_of_t Guess_ev.sexp_of_t cache in
  Sexp.save_hum "cache.sexp" s
