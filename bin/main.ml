open! Core
open Wordle_solver

let do_stuff num_guesses num_answers shuffle =
  let dictionary =
    Dictionary.create "guesses.txt" "answers.txt" ~num_guesses ~num_answers
      ~shuffle ()
  in

  let best_guess, expected_guesses =
    Solver.get_guess dictionary Information.empty
  in

  print_endline
    ("best guess: " ^ best_guess ^ "\nexpected guesses: "
    ^ Float.to_string expected_guesses);

  print_endline ("cache size: " ^ Int.to_string (Solver.cache_size ()));

  print_endline ("cache hits: " ^ Int.to_string (Solver.num_cache_hits ()))

let args = Sys.get_argv ()

let guesses, answers =
  if Array.length args = 2 then (0, Int.of_string args.(1))
  else (Int.of_string args.(1), Int.of_string args.(2))

let shuffle = Array.length args = 4
let () = do_stuff guesses answers shuffle
