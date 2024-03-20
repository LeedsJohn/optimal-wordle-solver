open! Core

let filter_answers answers guess res =
  List.filter answers ~f:(fun answer ->
      String.(guess <> answer && Evaluator.evaluate guess answer = res))

module Guess_ev = struct
  type t = string * float [@@deriving sexp]
end

let cache =
  if Sys_unix.file_exists_exn "cache.sexp" then
    Sexp.load_sexp "cache.sexp"
    |> Hashtbl.m__t_of_sexp (module String) Guess_ev.t_of_sexp
  else Hashtbl.create (module String)

let get_counts remaining_answers guess =
  let counts = Hashtbl.create (module String) in
  List.iter remaining_answers ~f:(fun answer ->
      Hashtbl.update counts (Evaluator.evaluate guess answer) ~f:(fun n ->
          match n with None -> 1 | Some n -> n + 1));
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
  let top_n = Top_n.create n in
  List.iter answers ~f:(fun word ->
      Top_n.add top_n word (expected_answers_remaining answers word));
  let score, _ = Top_n.get_min top_n in
  if Float.(score > 1.00001) then
    List.iter words ~f:(fun word ->
        if Float.(fst (Top_n.get_min top_n) > 1.00001) then
          Top_n.add top_n word (expected_answers_remaining answers word));
  Top_n.to_list top_n

let rec get_guess_aux ~guesses ~answers ~max_guesses ~exploration_rate =
  let eval_guess answers guess _best_so_far =
    let counts = get_counts answers guess in
    let num_answers = List.length answers in
    let total_guesses, _ =
      Hashtbl.fold counts ~init:(Float.zero, Float.zero)
        ~f:(fun ~key:result ~data:count (total_guesses, answers_examined) ->
          let open Float in
          let new_answers = filter_answers answers guess result in
          let _, expected_num_guesses =
            get_guess_aux ~guesses ~answers:new_answers
              ~max_guesses:Int.(max_guesses - 1)
              ~exploration_rate
          in
          let c = of_int count in
          ( total_guesses + ((1. + expected_num_guesses) * c),
            answers_examined + c ))
    in
    Float.(total_guesses / of_int num_answers)
  in
  if List.length answers = 0 then ("", 0.)
  else if max_guesses = 0 then ("", Float.infinity)
  else
    let guesses = get_guesses guesses answers exploration_rate in
    let res =
      List.fold guesses ~init:("", Float.infinity) ~f:(fun acc word ->
          let _best_word, best_score = acc in
          let score = eval_guess answers word best_score in
          if Float.(score < best_score) then (word, score) else acc)
    in
    res

let get_guess ~guesses ~answers ~max_guesses ~exploration_rate ~prev_results =
  match List.length prev_results with
  | 0 -> ("salet", 0.)
  | 1 -> Hashtbl.find_exn cache (List.hd_exn prev_results |> snd)
  | _ ->
      let answers =
        List.fold prev_results ~init:answers ~f:(fun acc (guess, res) ->
            filter_answers acc guess res)
      in
      get_guess_aux ~guesses ~answers ~max_guesses ~exploration_rate

let rec play_game_aux ~answer ~guesses ~answers ~max_guesses ~exploration_rate
    ~path =
  if List.length path > 0 && String.(fst (List.hd_exn path) = answer) then
    List.rev path |> List.map ~f:fst
  else
    let guess, _expected_score =
      get_guess ~guesses ~answers ~max_guesses ~exploration_rate
        ~prev_results:path
    in
    let max_guesses = max_guesses - 1 in
    let res = Evaluator.evaluate guess answer in
    let answers = filter_answers answers guess res in
    play_game_aux ~answer ~guesses ~answers ~max_guesses ~exploration_rate
      ~path:((guess, res) :: path)

let play_game ~answer ~exploration_rate =
  let dictionary = Dictionary.create "guesses.txt" "answers.txt" () in
  let guesses, answers =
    (Dictionary.get_words dictionary, Dictionary.get_answers dictionary)
  in
  play_game_aux ~answer ~guesses ~answers ~max_guesses:5 ~exploration_rate
    ~path:[]

let get_total_guesses possible_answers exploration_rate =
  List.foldi possible_answers ~init:0 ~f:(fun _i acc answer ->
      let path = play_game ~answer ~exploration_rate in
      let acc = acc + List.length path in
      printf "%s: " answer;
      print_s [%sexp (path : string list)];
      Out_channel.flush Out_channel.stdout;
      acc)

let rec play_game_interactive_aux guesses answers max_guesses prev_results =
  printf "Enter your guess or press enter to get a recommendation: ";
  Out_channel.flush Out_channel.stdout;
  let guess = In_channel.input_line_exn In_channel.stdin in
  let guess =
    if String.length guess <> 0 then guess
    else
      let recommended_guess, ev =
        get_guess ~guesses ~answers ~max_guesses ~exploration_rate:40
          ~prev_results
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
    play_game_interactive_aux guesses answers (max_guesses - 1)
      ((guess, result) :: prev_results)

let play_game_interactive () =
  printf "Enter your guess: ";
  Out_channel.flush Out_channel.stdout;
  let guess = In_channel.input_line_exn In_channel.stdin in
  printf "Enter your result: ";
  Out_channel.flush Out_channel.stdout;
  let result = In_channel.input_line_exn In_channel.stdin in
  let dictionary = Dictionary.create "guesses.txt" "answers.txt" () in
  let guesses, answers =
    (Dictionary.get_words dictionary, Dictionary.get_answers dictionary)
  in
  play_game_interactive_aux guesses answers 5 [ (guess, result) ]

let create_cache ~guesses ~answers ~exploration_rate =
  let guess = "salet" in
  List.iteri answers ~f:(fun i answer ->
      printf "%d\n" i;
      Out_channel.flush Out_channel.stdout;
      let result = Evaluator.evaluate guess answer in
      Hashtbl.update cache result ~f:(fun res ->
          match res with
          | Some res -> res
          | None ->
              let answers = filter_answers answers "salet" result in
              get_guess_aux ~guesses ~answers ~max_guesses:5 ~exploration_rate));
  let s = Hashtbl.sexp_of_t String.sexp_of_t Guess_ev.sexp_of_t cache in
  Sexp.save_hum "cache.sexp" s
