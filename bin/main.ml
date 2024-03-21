open! Core
open Wordle_solver

let play_game_interactive_command =
  Command.basic ~summary:"Interactively play a game of Wordle"
    (Command.Param.return (fun () -> Solver.play_game_interactive ()))

let make_cache_command =
  Command.basic ~summary:"Fill the cache of the best second guess"
    (let%map_open.Command exploration_rate =
       flag "--exploration-rate"
         (optional_with_default 20 int)
         ~doc:"How many words to try at each decision point (default: 20)"
     in
     fun () ->
       let d = Dictionary.create "guesses.txt" "answers.txt" () in
       let guesses, answers =
         (Dictionary.get_words d, Dictionary.get_answers d)
       in
       Solver.create_cache ~guesses ~answers ~exploration_rate)

let test_command =
  Command.basic
    ~summary:"Get the average number of guesses to solve all possible answers"
    (let%map_open.Command exploration_rate =
       flag "--exploration-rate"
         (optional_with_default 20 int)
         ~doc:"How many words to try at each decision point (default: 20)"
     in
     fun () ->
       let answers =
         Dictionary.get_answers
           (Dictionary.create "guesses.txt" "answers.txt" ~shuffle:false ())
       in
       let num_answers = List.length answers in
       let total_guesses = Solver.get_total_guesses answers exploration_rate in
       printf
         "Average number of guesses: %f (%d guesses across %d possible answers)\n"
         Float.(of_int total_guesses / of_int num_answers)
         total_guesses num_answers)

let get_guess_command =
  Command.basic ~summary:"Get a guess for a Wordle game"
    (let%map_open.Command num_words = anon ("num_words" %: int)
     and answer_percent =
       flag "-gp"
         (map_flag (optional_with_default 0.2 float) ~f:(fun n ->
              Float.(min 1.0 n |> max 0.)))
         ~doc:"percent of words that are a valid answer"
     and max_guesses =
       flag ~aliases:[ "--max-guesses" ] "-mg"
         (optional_with_default 5 int)
         ~doc:
           "Maximum number of guesses to explore before giving up (default 5). \
            Note: Setting too low may result in either not finding a guess or \
            finding a suboptimal guess"
     and exploration_rate =
       flag ~aliases:[ "--exploration-rate" ] "-er"
         (optional_with_default 20 int)
         ~doc:
           "How many guesses at each level to explore (default 20). The best \
            guesses are determined by finding the expected number of answers \
            to be eliminated by making a guess. Note: Setting too low may \
            result in finding a suboptimal guess"
     and no_shuffle =
       flag ~aliases:[ "--no-shuffle" ] "-ns" no_arg
         ~doc:
           "Specify to not shuffle the dictionary and choose guesses / answers \
            alphabetically"
     in
     fun () ->
       let num_answers =
         Float.(of_int num_words * answer_percent |> round_up) |> Int.of_float
       in
       let num_guesses = num_words - num_answers in
       let dictionary =
         Dictionary.create "guesses.txt" "answers.txt" ~num_guesses ~num_answers
           ~shuffle:(not no_shuffle) ()
       in
       let guesses, answers =
         (Dictionary.get_words dictionary, Dictionary.get_answers dictionary)
       in
       let best_guess, expected_guesses =
         Solver.get_guess ~guesses ~answers ~max_guesses ~exploration_rate
           ~prev_results:[]
       in
       printf "Best guess: %S (Expected guesses: %f)\n" best_guess.word
         expected_guesses)

let command =
  Command.group ~summary:"Wordle Commands"
    [
      ("play", play_game_interactive_command);
      ("get-guess", get_guess_command);
      ("test", test_command);
      ("make-cache", make_cache_command);
    ]

let () = Command_unix.run command
