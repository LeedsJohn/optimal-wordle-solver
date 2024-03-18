open! Core
open Wordle_solver

(* let i = (Sys.get_argv ()).(1) |> Int.of_string
   let a = 385 * (i - 1)
   let b = if i <> 6 then a + 385 else 0
   let d = Dictionary.create "guesses.txt" "answers.txt" ~shuffle:false ()
   let answers = List.slice (Dictionary.get_answers d) a b
   let n = Solver.get_total_guesses answers

   let () =
     printf "Total guesses: %d (total answers: %d)\n" n (List.length answers) *)

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
       let best_guess, expected_guesses =
         Solver.get_guess ~dictionary ~information:Information.empty
           ~max_guesses ~exploration_rate
       in
       printf
         "Best guess: %S (Expected guesses: %f)\n\
          Cache size: %d (Cache hits: %d)\n"
         best_guess expected_guesses (Solver.cache_size ())
         (Solver.num_cache_hits ()))

let () = Command_unix.run get_guess_command
