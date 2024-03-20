open! Core

module Word_pair = struct
  type t = string * string [@@deriving compare, hash, sexp_of]
end

let cache = Hashtbl.create (module Word_pair)

let evaluate guess answer =
  match Hashtbl.find cache (guess, answer) with
  | Some res -> res
  | None ->
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
      let res = String.of_array res in
      Hashtbl.add_exn cache ~key:(guess, answer) ~data:res;
      res

let%expect_test "evaluating" =
  let res = evaluate "aaaaa" "bbbbb" in
  print_endline res;
  [%expect {|xxxxx|}];
  let res = evaluate "aaaaa" "aaaaa" in
  print_endline res;
  [%expect {|ggggg|}];
  let res = evaluate "abcde" "acdeb" in
  print_endline res;
  [%expect {|gyyyy|}];
  let res = evaluate "aabaa" "xxaxa" in
  print_endline res;
  [%expect {|yxxxg|}]
