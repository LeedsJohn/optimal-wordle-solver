open! Core

(* Each word is represented as a base 26 integer *)

type t = int [@@deriving sexp]

let compare t1 t2 = Int.compare (Int.abs t1) (Int.abs t2)
let equal t1 t2 = Int.equal (Int.abs t1) (Int.abs t2)
let hash t = Int.abs t
let hash_fold_t state t = Int.hash_fold_t state (Int.abs t)

let of_string s =
  String.foldi s ~init:0 ~f:(fun i acc c ->
      let base = Int.pow 26 i in
      let n = Char.to_int c - Char.to_int 'a' in
      acc + (base * n))

let create word ~possible_answer =
  let num = of_string word in
  if possible_answer then num else 0 - num

let empty_word = create "" ~possible_answer:false

let get_char t i =
  let n = Int.abs t / Int.pow 26 i % 26 in
  Char.of_int_exn (n + Char.to_int 'a')

let to_string t = List.map (List.range 0 5) ~f:(get_char t) |> String.of_list

let count_occurrences t c =
  List.count (List.range 0 5) ~f:(fun i -> Char.(get_char t i = c))

let possible_answer t = t >= 0

let%expect_test "encoding words" =
  print_endline (of_string "hello" |> to_string);
  [%expect {| hello |}];
  print_endline (of_string "aaaaa" |> to_string);
  [%expect {| aaaaa |}];
  print_endline (of_string "zzzzz" |> to_string);
  [%expect {| zzzzz |}];
  print_endline (of_string "zazaz" |> to_string);
  [%expect {| zazaz |}];
  ()
