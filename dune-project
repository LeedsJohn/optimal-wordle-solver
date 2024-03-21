(lang dune 3.11)

(name wordle_solver)

(generate_opam_files true)

(source
 (github leedsjohn/optimal-wordle-solver))

(authors "John Leeds")

(maintainers "John Leeds")

(documentation https://github.com/LeedsJohn/optimal-wordle-solver)

(package
 (name wordle_solver)
 (synopsis "Project to solve Wordle puzzles optimizing for the lowest number of average guesses.")
 (depends core core_unix dune ocaml ppx_jane))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
