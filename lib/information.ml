open! Core

let char_to_int c = Char.to_int c - Char.to_int 'a'
let char_of_int n = Char.of_int_exn (n + Char.to_int 'a')

module type Word_size = sig
  val word_size : int
end

module Bit_set (M : Word_size) = struct
  type t = int [@@deriving compare, hash, sexp]

  let get t pos =
    let shifted = Int.shift_right_logical t (M.word_size * pos) in
    let mask =
      Int.shift_right_logical (Int.bit_not 0) (Int.num_bits - M.word_size)
    in
    Int.bit_and shifted mask

  let set t pos x =
    let x = Int.shift_left x (pos * M.word_size) in
    let one = Int.bit_not 0 in
    let mask = Int.shift_left one M.word_size |> Int.bit_not in
    let mask = Int.shift_left mask (pos * M.word_size) |> Int.bit_not in
    let t = Int.bit_and t mask in
    Int.bit_or t x

  let bit_or t1 t2 = Int.bit_or t1 t2
end

module Greens = struct
  module M = Bit_set (struct
    let word_size = 5
  end)

  type t = M.t [@@deriving compare, hash, sexp]

  let set t pos c = M.set t pos (char_to_int c + 1)

  let get t pos =
    let n = M.get t pos in
    if n = 0 then None else Some (char_of_int (n - 1))

  let bit_or = M.bit_or

  let count_occurrences t c =
    List.fold (List.range 0 5) ~init:0 ~f:(fun acc i ->
        match get t i with
        | None -> acc
        | Some c2 -> acc + Bool.to_int Char.(c = c2))
end

module Min_counts = struct
  module M = Bit_set (struct
    let word_size = 2
  end)

  type t = M.t [@@deriving compare, hash, sexp]

  let set t c n = M.set t (char_to_int c) n
  let get t c = M.get t (char_to_int c)
end

module Found_max = struct
  module M = Bit_set (struct
    let word_size = 1
  end)

  type t = M.t [@@deriving compare, hash, sexp]

  let set t c = M.set t (char_to_int c) 1
  let get t c = M.get t (char_to_int c) = 1
  let bit_or = M.bit_or
end

module Bad_characters = struct
  module M = Bit_set (struct
    let word_size = 1
  end)

  type t = M.t list [@@deriving compare, hash, sexp]

  let empty = [ 0; 0; 0; 0; 0 ]

  let get t i c =
    let bs = List.nth_exn t i in
    M.get bs (char_to_int c) = 1

  let set_val t i c n =
    let l, r = List.split_n t i in
    l @ [ M.set (List.hd_exn r) (char_to_int c) n ] @ List.tl_exn r

  let set t i c = set_val t i c 1

  let erase t i =
    let l, r = List.split_n t i in
    l @ [ 0 ] @ List.tl_exn r

  let erase_all_characters t c =
    List.fold (List.range 0 5) ~init:t ~f:(fun acc i -> set_val acc i c 0)

  let bit_or t1 t2 = List.map2_exn t1 t2 ~f:(fun n1 n2 -> Int.bit_or n1 n2)
end

type t = {
  greens : Greens.t;
  min_counts : Min_counts.t;
  found_max : Found_max.t;
  bad_characters : Bad_characters.t;
}
[@@deriving compare, hash, sexp]

let empty =
  {
    greens = 0;
    min_counts = 0;
    found_max = 0;
    bad_characters = Bad_characters.empty;
  }

let can_word_be_answer t word =
  let green_check () =
    String.for_alli word ~f:(fun i c ->
        let green_pos = Greens.get t.greens i in
        match green_pos with None -> true | Some c2 -> Char.(c = c2))
  in
  let bad_character_check () =
    String.for_alli word ~f:(fun i c ->
        not (Bad_characters.get t.bad_characters i c))
  in
  let count_check () =
    List.for_all (List.range 0 26) ~f:(fun i ->
        let c = char_of_int i in
        let count = String.count word ~f:(Char.equal c) in
        let min_count = Min_counts.get t.min_counts c in
        let capped = Found_max.get t.found_max c in
        (capped && count = min_count) || ((not capped) && count >= min_count))
  in
  bad_character_check () && green_check () && count_check ()

let filter_bad_characters t =
  let bad_characters =
    List.fold (List.range 0 5) ~init:t.bad_characters ~f:(fun acc i ->
        if Option.is_some (Greens.get t.greens i) then
          Bad_characters.erase acc i
        else acc)
  in
  let get_num_green c =
    List.fold (List.range 0 5) ~init:0 ~f:(fun acc i ->
        match Greens.get t.greens i with
        | None -> acc
        | Some c2 -> acc + Bool.to_int Char.(c = c2))
  in
  let bad_characters =
    List.fold (List.range 0 26) ~init:bad_characters ~f:(fun acc i ->
        let c = char_of_int i in
        if
          Found_max.get t.found_max c
          && get_num_green c = Min_counts.get t.min_counts c
        then Bad_characters.erase_all_characters acc c
        else acc)
  in
  { t with bad_characters }

let get_info_aux guess result =
  let greens =
    String.foldi result ~init:0 ~f:(fun i acc c ->
        if Char.(c = 'g') then Greens.set acc i (String.unsafe_get guess i)
        else acc)
  in
  let bad_characters =
    String.foldi result ~init:Bad_characters.empty ~f:(fun i acc c ->
        let c2 = String.unsafe_get guess i in
        if Char.(c <> 'g') then Bad_characters.set acc i c2 else acc)
  in
  let char_set = String.to_list guess |> Set.of_list (module Char) in
  let times_appeared =
    Set.fold char_set
      ~init:(Map.empty (module Char))
      ~f:(fun acc c ->
        Map.add_exn acc ~key:c ~data:(String.count guess ~f:(Char.equal c)))
  in
  let times_gray =
    Set.fold char_set
      ~init:(Map.empty (module Char))
      ~f:(fun acc c ->
        Map.add_exn acc ~key:c
          ~data:
            (String.foldi guess ~init:0 ~f:(fun i acc c2 ->
                 acc
                 + Bool.to_int Char.(c = c2 && String.unsafe_get result i = 'x'))))
  in
  let min_counts =
    Map.fold2 times_appeared times_gray ~init:0 ~f:(fun ~key ~data:x acc ->
        let appearances, num_gray =
          Map.Merge_element.values x ~left_default:0
            ~right_default:0 (* maps will always have the same keys *)
        in
        Min_counts.set acc key (appearances - num_gray))
  in
  let found_max =
    Map.fold times_gray ~init:0 ~f:(fun ~key:c ~data:num_gray acc ->
        if num_gray <> 0 then Found_max.set acc c else acc)
  in
  filter_bad_characters { greens; min_counts; found_max; bad_characters }

let cache = Hashtbl.create (module String)

let get_info guess answer =
  let result = Evaluator.evaluate guess answer in
  match Hashtbl.find cache (guess ^ result) with
  | Some res -> res
  | None ->
      let res = get_info_aux guess result in
      Hashtbl.add_exn cache ~key:(guess ^ result) ~data:res;
      res

let add_information t ~guess ~answer =
  let new_info = get_info guess answer in
  let greens = Greens.bit_or t.greens new_info.greens in
  let bad_characters =
    Bad_characters.bit_or t.bad_characters new_info.bad_characters
  in
  let min_counts =
    List.fold (List.range 0 26) ~init:0 ~f:(fun acc i ->
        let c = char_of_int i in
        let max_val =
          Int.max
            (Min_counts.get t.min_counts c)
            (Min_counts.get new_info.min_counts c)
        in
        Min_counts.set acc c max_val)
  in
  let found_max = Found_max.bit_or t.found_max new_info.found_max in
  let found_max =
    List.fold (List.range 0 26) ~init:found_max ~f:(fun acc i ->
        let c = char_of_int i in
        if
          List.for_all (List.range 0 5) ~f:(fun i ->
              Option.is_some (Greens.get greens i)
              || Bad_characters.get bad_characters i c)
        then Found_max.set acc c
        else acc)
  in
  filter_bad_characters { greens; min_counts; found_max; bad_characters }

let all_greens t =
  List.for_all (List.range 0 5) ~f:(fun i ->
      Option.is_some (Greens.get t.greens i))

let is_guess_useless t guess =
  let new_letter_count =
    String.exists guess ~f:(fun c ->
        (not (Found_max.get t.found_max c))
        && String.count guess ~f:(Char.equal c) > Min_counts.get t.min_counts c)
  in
  if new_letter_count then false
  else
    let green_spot i = Option.is_some (Greens.get t.greens i) in
    let known_character c =
      Found_max.get t.found_max c
      && Greens.count_occurrences t.greens c = Min_counts.get t.min_counts c
    in
    let bad_character i c = Bad_characters.get t.bad_characters i c in
    String.for_alli guess ~f:(fun i c ->
        green_spot i || known_character c || bad_character i c)

(* let spot_is_green i = Option.is_some (Greens.get t.greens i) in
   let bad_character i c =
     let already_guessed = Bad_characters.get t.bad_characters i c in
     let no_unknown_spots =
       let count = Min_counts.get t.min_counts c in
       let num_found_spots =
         List.fold (List.range 0 5) ~init:0 ~f:(fun acc i ->
             match Greens.get t.greens i with
             | None -> acc
             | Some c2 -> acc + Bool.to_int Char.(c = c2))
       in
       count = num_found_spots
     in
     already_guessed || no_unknown_spots
   in
   let guessed_char_positions =
     String.for_alli guess ~f:(fun i c -> spot_is_green i || bad_character i c)
   in
   let bad_char_counts =
     String.for_all guess ~f:(fun c ->
         let count_in_guess = String.count guess ~f:(Char.equal c) in
         let capped = Found_max.get t.found_max c in
         if capped then true else count_in_guess <= Min_counts.get t.min_counts c)
   in
   guessed_char_positions && bad_char_counts *)

let char_guessed t c =
  Min_counts.get t.min_counts c <> 0 || Found_max.get t.found_max c

let show t =
  let greens =
    List.fold (List.range 0 5) ~init:"Greens: " ~f:(fun acc i ->
        let c = Greens.get t.greens i in
        let c = match c with None -> "_" | Some c -> Char.to_string c in
        acc ^ c)
  in
  let get_bad_character i =
    List.fold (List.range 0 26)
      ~init:("\t" ^ Int.to_string (i + 1) ^ ": ")
      ~f:(fun acc j ->
        if Bad_characters.get t.bad_characters i (char_of_int j) then
          acc ^ Char.to_string (char_of_int j) ^ ", "
        else acc)
  in
  let counts =
    List.fold (List.range 0 26) ~init:"Counts: " ~f:(fun acc i ->
        let c = char_of_int i in
        let capped = Found_max.get t.found_max c in
        let count = Min_counts.get t.min_counts c in
        let s = acc ^ Char.to_string c ^ " " ^ Int.to_string count in
        if capped then s ^ " (M), " else if count <> 0 then s ^ ", " else acc)
  in
  print_endline greens;
  print_endline "Bad characters:";
  List.iter (List.range 0 5) ~f:(fun i -> print_endline (get_bad_character i));
  print_endline counts

let%expect_test "get info" =
  let info = get_info "aaabb" "aaabb" in
  show info;
  [%expect
    {|
    Greens: aaabb
    Bad characters:
    	1:
    	2:
    	3:
    	4:
    	5:
    Counts: a 3, b 2, |}];
  let info = get_info "stuff" "foots" in
  show info;
  [%expect
    {|
    Greens: _____
    Bad characters:
    	1: s,
    	2: t,
    	3:
    	4: f,
    	5: f,
    Counts: f 1 (M), s 1, t 1, u 0 (M), |}]

let%expect_test "merge info" =
  let info = get_info "soare" "erupt" in
  let info = add_information info ~guess:"clint" ~answer:"erupt" in
  (* bad characters for the 5th letter are erased once the t is found *)
  show info;
  [%expect
    {|
    Greens: ____t
    Bad characters:
    	1:
    	2:
    	3:
    	4: r,
    	5:
    Counts: a 0 (M), c 0 (M), e 1, i 0 (M), l 0 (M), n 0 (M), o 0 (M), r 1, s 0 (M), t 1, |}];
  let info = add_information info ~guess:"stuff" ~answer:"erupt" in
  (* bad characters stay erased for 5th letter *)
  show info;
  [%expect
    {|
    Greens: __u_t
    Bad characters:
    	1:
    	2: t,
    	3:
    	4: r,
    	5:
    Counts: a 0 (M), c 0 (M), e 1, f 0 (M), i 0 (M), l 0 (M), n 0 (M), o 0 (M), r 1, s 0 (M), t 1, u 1, |}];
  let info = add_information empty ~guess:"abase" ~answer:"abate" in
  let info = add_information info ~guess:"abbey" ~answer:"abate" in
  show info;
  [%expect
    {|
    Greens: aba_e
    Bad characters:
    	1:
    	2:
    	3:
    	4:
    	5:
    Counts: a 2, b 1 (M), e 1 (M), s 0 (M), y 0 (M), |}];
  ()

let%expect_test "word elimination" =
  let t = get_info "abcde" "zzzye" in
  let bad_green = can_word_be_answer t "fffff" in
  print_s [%sexp (bad_green : bool)];
  [%expect {| false |}];
  let t = get_info "abcde" "zzazz" in
  let bad_yellow = can_word_be_answer t "fffff" in
  print_s [%sexp (bad_yellow : bool)];
  [%expect {| false |}];
  let t = get_info "abcde" "fghij" in
  let good_answer = can_word_be_answer t "zzazz" in
  print_s [%sexp (good_answer : bool)];
  [%expect {| true |}];
  let t = get_info "oooaa" "bbboo" in
  let above_max_count = can_word_be_answer t "bbooo" in
  print_s [%sexp (above_max_count : bool)];
  [%expect {| false |}];
  let t =
    add_information (get_info "crane" "erupt") ~guess:"defer" ~answer:"erupt"
  in
  let good_answer_press = can_word_be_answer t "press" in
  print_s [%sexp (good_answer_press : bool)];
  [%expect {| true |}];
  let t = add_information t ~guess:"press" ~answer:"erupt" in
  let good_answer_erupt = can_word_be_answer t "erupt" in
  print_s [%sexp (good_answer_erupt : bool)];
  [%expect {| true |}];
  let t = add_information empty ~guess:"abbey" ~answer:"abate" in
  print_s [%sexp (can_word_be_answer t "aback" : bool)];
  [%expect {| false |}];
  ()

let%expect_test "is guess useless" =
  let t = add_information empty ~guess:"abcde" ~answer:"awxyz" in
  print_s [%sexp (is_guess_useless t "aedcb" : bool)];
  [%expect {| true |}];
  let t = add_information empty ~guess:"abcde" ~answer:"vwxya" in
  print_s [%sexp (is_guess_useless t "abcde" : bool)];
  [%expect {| true |}];
  print_s [%sexp (is_guess_useless t "edcba" : bool)];
  [%expect {| false |}];
  print_s [%sexp (is_guess_useless empty "aback" : bool)];
  [%expect {| false |}];
  ()

let%expect_test "bit set" =
  let n = 0 in
  print_s [%sexp (Greens.get n 1 : char option)];
  [%expect {| () |}];
  let n = Greens.set n 0 'a' in
  print_s [%sexp (Greens.get n 0 : char option)];
  [%expect {| (a) |}];
  let n = Greens.set n 4 'z' in
  print_s [%sexp (Greens.get n 4 : char option)];
  [%expect {| (z) |}];
  let n2 = Greens.set 0 2 'b' in
  let n = Greens.bit_or n n2 in
  List.iter (List.range 0 5) ~f:(fun i ->
      print_s [%sexp (Greens.get n i : char option)]);
  [%expect {|
  (a)
  ()
  (b)
  ()
  (z) |}];
  let bad = Bad_characters.empty in
  let bad = Bad_characters.set bad 0 'a' in
  let bad = Bad_characters.set bad 2 'd' in
  let bad = Bad_characters.set bad 4 'z' in
  print_s [%sexp (Bad_characters.get bad 0 'a' : bool)];
  print_s [%sexp (Bad_characters.get bad 2 'd' : bool)];
  print_s [%sexp (Bad_characters.get bad 4 'z' : bool)];
  print_s [%sexp (Bad_characters.get bad 0 'z' : bool)];
  [%expect {|
    true
    true
    true
    false |}];
  ()
