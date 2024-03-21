open! Core

module Word_pair = struct
  type t = Word.t * Word.t [@@deriving compare, sexp_of]

  let hash ((w1, w2) : t) = w1.id + (w2.id * (Word.max_id + 1))
end

let cache = Hashtbl.create (module Word_pair)

let evaluate_aux guess answer =
  let res = Array.create ~len:5 'x' in
  let count_found_characters c =
    String.foldi guess ~init:0 ~f:(fun i acc c2 ->
        acc + Bool.to_int Char.(c = c2 && res.(i) <> 'x'))
  in
  String.iteri guess ~f:(fun i c ->
      if Char.(c = String.unsafe_get answer i) then res.(i) <- 'g');

  String.iteri guess ~f:(fun i c ->
      let count = String.count answer ~f:(Char.equal c) in
      let found_characters = count_found_characters c in
      if Char.(res.(i) <> 'g') && count > 0 && found_characters < count then
        res.(i) <- 'y');
  String.of_array res

let evaluate guess answer =
  match Hashtbl.find cache (guess, answer) with
  | Some res -> res
  | None ->
      let res = evaluate_aux guess.word answer.word in
      Hashtbl.add_exn cache ~key:(guess, answer) ~data:res;
      res

let%expect_test "evaluating" =
  let res = evaluate_aux "aaaaa" "bbbbb" in
  print_endline res;
  [%expect {|xxxxx|}];
  let res = evaluate_aux "aaaaa" "aaaaa" in
  print_endline res;
  [%expect {|ggggg|}];
  let res = evaluate_aux "abcde" "acdeb" in
  print_endline res;
  [%expect {|gyyyy|}];
  let res = evaluate_aux "aabaa" "xxaxa" in
  print_endline res;
  [%expect {|yxxxg|}]
