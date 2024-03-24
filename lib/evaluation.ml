open! Core

type t = int [@@deriving sexp]

let compare t1 t2 = Int.compare t1 t2
let hash t = t
let hash_fold_t state t = Int.hash_fold_t state t
let c_to_num = function 'x' -> 0 | 'y' -> 1 | _ -> 2
let num_to_c = function 0 -> 'x' | 1 -> 'y' | _ -> 'g'
let equal (t1 : t) (t2 : t) = Int.equal t1 t2

let of_string cells =
  String.foldi cells ~init:0 ~f:(fun i acc c ->
      acc + (c_to_num c * Int.pow 3 i))

let get t ~pos = num_to_c (t / Int.pow 3 pos % 3)

let to_string t =
  List.map (List.range 0 5) ~f:(fun pos -> get t ~pos) |> String.of_list

let%expect_test "encoding results" =
  print_endline (of_string "ggggg" |> to_string);
  [%expect {| ggggg |}];
  print_endline (of_string "xyggg" |> to_string);
  [%expect {| xyggg |}];
  print_endline (of_string "gyxxx" |> to_string);
  [%expect {| gyxxx |}];
  ()

module Word_pair = struct
  type t = Word.t * Word.t [@@deriving sexp_of]

  let compare ((w1, w2) : t) ((w3, w4) : t) =
    if Word.compare w1 w3 <> 0 then Word.compare w1 w3 else Word.compare w2 w4

  let hash (w1, w2) = Word.hash w1 * Word.hash w2 |> Int.abs
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
  of_string (String.of_array res)

let evaluate guess answer =
  match Hashtbl.find cache (guess, answer) with
  | Some res -> res
  | None ->
      let res = evaluate_aux (Word.to_string guess) (Word.to_string answer) in
      Hashtbl.add_exn cache ~key:(guess, answer) ~data:res;
      res

let%expect_test "evaluating" =
  let res = to_string (evaluate_aux "aaaaa" "bbbbb") in
  print_endline res;
  [%expect {|xxxxx|}];
  let res = to_string (evaluate_aux "aaaaa" "aaaaa") in
  print_endline res;
  [%expect {|ggggg|}];
  let res = to_string (evaluate_aux "abcde" "acdeb") in
  print_endline res;
  [%expect {|gyyyy|}];
  let res = to_string (evaluate_aux "aabaa" "xxaxa") in
  print_endline res;
  [%expect {|yxxxg|}]
