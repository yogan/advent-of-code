open Aoc
open OUnit2

let print_tuple fn (a, b) = Printf.sprintf "(%s, %s)" (fn a) (fn b)
let print_list fn lst = "[" ^ String.concat "; " (List.map fn lst) ^ "]"

let print_set fn lst =
  "{" ^ String.concat "; " (PosSet.to_list lst |> List.map fn) ^ "}"

let print_string x = "\"" ^ x ^ "\""
let print_list_of_string = print_list print_string
let print_tuple_of_list_of_string = print_tuple print_list_of_string
let print_pos (r, c) = Printf.sprintf "(%d/%d)" r c
let print_set_of_pos = print_set print_pos
let print_list_of_pos = print_list print_pos

let assert_equal_posset expected actual =
  assert_equal expected actual ~cmp:PosSet.equal ~printer:print_set_of_pos

let sample_input =
  [
    "#######";
    "#...#.#";
    "#.....#";
    "#..OO@#";
    "#..O..#";
    "#.....#";
    "#######";
    "";
    "<vv<<^^";
    "<<^^";
  ]

let test_split_list _ =
  assert_equal
    ([ "a1"; "a2" ], [ "b1"; "b2" ])
    (split_list "" [ "a1"; "a2"; ""; "b1"; "b2" ])
    ~printer:print_tuple_of_list_of_string

let test_find_chars _ =
  let grid = [ "###"; "#.#"; "O.#" ] in
  assert_equal_posset
    ([ (0, 0); (0, 1); (0, 2); (1, 0); (1, 2); (2, 2) ] |> PosSet.of_list)
    (find_chars grid '#');
  assert_equal_posset ([ (2, 0) ] |> PosSet.of_list) (find_chars grid 'O')

let test_parse_lines _ =
  let { robot; walls; boxes; moves } = parse_lines sample_input in
  assert_equal (3, 5) robot ~printer:print_pos;
  assert_equal 25 (walls |> PosSet.to_seq |> Seq.length) ~printer:string_of_int;
  assert_equal_posset ([ (3, 3); (3, 4); (4, 3) ] |> PosSet.of_list) boxes;
  assert_equal "<vv<<^^<<^^" moves ~printer:print_string

let test_find_boxes _ =
  let pr = print_list_of_pos in
  let pos = (1, 1) in
  let boxes = [ (1, 2); (1, 3); (2, 1) ] |> PosSet.of_list in
  assert_equal [ (1, 2); (1, 3) ] (find_boxes boxes pos '>') ~printer:pr;
  assert_equal [ (2, 1) ] (find_boxes boxes pos 'v') ~printer:pr;
  assert_equal [] (find_boxes boxes pos '<') ~printer:pr;
  assert_equal [] (find_boxes boxes pos '^') ~printer:pr

let test_move_boxes _ =
  let boxes = [ (1, 2); (1, 3); (9, 9) ] |> PosSet.of_list in
  let boxes_to_move = [ (1, 2); (1, 3) ] in
  assert_equal_posset
    ([ (1, 3); (1, 4); (9, 9) ] |> PosSet.of_list)
    (move_boxes boxes boxes_to_move '>');
  assert_equal_posset
    ([ (2, 2); (2, 3); (9, 9) ] |> PosSet.of_list)
    (move_boxes boxes boxes_to_move 'v');
  assert_equal_posset
    ([ (1, 1); (1, 2); (9, 9) ] |> PosSet.of_list)
    (move_boxes boxes boxes_to_move '<');
  assert_equal_posset
    ([ (0, 2); (0, 3); (9, 9) ] |> PosSet.of_list)
    (move_boxes boxes boxes_to_move '^')

let suite =
  "AoC Tests"
  >::: [
         "test_split_list" >:: test_split_list;
         "test_find_chars" >:: test_find_chars;
         "test_parse_lines" >:: test_parse_lines;
         "test_find_boxes" >:: test_find_boxes;
         "test_move_boxes" >:: test_move_boxes;
       ]

let () = run_test_tt_main suite
