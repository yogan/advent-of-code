open Aoc
open OUnit2

let print_tuple fn (a, b) = Printf.sprintf "(%s, %s)" (fn a) (fn b)
let print_list fn lst = "[" ^ String.concat "; " (List.map fn lst) ^ "]"

let print_posset fn lst =
  "{" ^ String.concat "; " (PosSet.to_list lst |> List.map fn) ^ "}"

let print_wideposset fn lst =
  "{" ^ String.concat "; " (WidePosSet.to_list lst |> List.map fn) ^ "}"

let print_string x = "\"" ^ x ^ "\""
let print_list_of_string = print_list print_string
let print_tuple_of_list_of_string = print_tuple print_list_of_string
let print_pos (r, c) = Printf.sprintf "(r=%d, c=%d)" r c
let print_wide_pos (r, cl, cr) = Printf.sprintf "(r=%d, cl=%d, cr=%d)" r cl cr
let print_list_of_wide_pos = print_list print_wide_pos
let print_set_of_pos = print_posset print_pos
let print_set_of_wide_pos = print_wideposset print_wide_pos

let assert_equal_posset expected actual =
  assert_equal expected actual ~cmp:PosSet.equal ~printer:print_set_of_pos

let assert_equal_wideposset expected actual =
  assert_equal expected actual ~cmp:WidePosSet.equal
    ~printer:print_set_of_wide_pos

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
  assert_equal_wideposset
    ([ (3, 3, 3); (3, 4, 4); (4, 3, 3) ] |> WidePosSet.of_list)
    boxes;
  assert_equal "<vv<<^^<<^^" moves ~printer:print_string

let test_widen_walls _ =
  assert_equal_posset
    ([ (0, 0); (0, 1); (0, 2); (0, 3); (1, 2); (1, 3) ] |> PosSet.of_list)
    (widen_walls ([ (0, 0); (0, 1); (1, 1) ] |> PosSet.of_list))

let test_widen_boxes _ =
  assert_equal_wideposset
    ([ (0, 0, 1); (0, 2, 3); (1, 2, 3) ] |> WidePosSet.of_list)
    (widen_boxes ([ (0, 0, 0); (0, 1, 1); (1, 1, 1) ] |> WidePosSet.of_list))

let test_find_boxes _ =
  let assert_eq r e = assert_equal e r ~printer:print_list_of_wide_pos in
  let pos = (1, 1) in
  let boxes = [ (1, 2, 2); (1, 3, 3); (2, 1, 1) ] |> WidePosSet.of_list in
  assert_eq (find_boxes boxes pos '>') [ (1, 2, 2); (1, 3, 3) ];
  assert_eq (find_boxes boxes pos 'v') [ (2, 1, 1) ];
  assert_eq (find_boxes boxes pos '<') [];
  assert_eq (find_boxes boxes pos '^') []

let test_find_boxes_wide_horizontal _ =
  let assert_eq r e = assert_equal e r ~printer:print_list_of_wide_pos in
  let find = find_boxes_wide_horizontal in
  let boxes =
    [ (1, 2, 3); (1, 4, 5); (1, 7, 8); (2, 1, 2) ] |> WidePosSet.of_list
  in
  assert_eq (find boxes (1, 1) '<') [];
  assert_eq (find boxes (1, 1) '>') [ (1, 2, 3); (1, 4, 5) ];
  assert_eq (find boxes (1, 6) '<') [ (1, 4, 5); (1, 2, 3) ];
  assert_eq (find boxes (1, 6) '>') [ (1, 7, 8) ]

let test_find_boxes_wide_vertical _ =
  let assert_eq r e =
    assert_equal_wideposset (e |> WidePosSet.of_list) (r |> WidePosSet.of_list)
  in
  let find = find_boxes_wide_vertical in
  (*   1234567
   * 1 .[]..{}
   * 2 ..[]...
   * 3 .[][]..
   * 4 ..[]...
   * 5 ...@... *)
  let r, c = (5, 4) in
  let boxes =
    [ (1, 2, 3); (1, 6, 7); (2, 3, 4); (3, 2, 3); (3, 4, 5); (4, 3, 4) ]
    |> WidePosSet.of_list
  in
  let expected = [ (1, 2, 3); (2, 3, 4); (3, 2, 3); (3, 4, 5); (4, 3, 4) ] in
  assert_eq (find boxes r c c '^') expected;
  assert_eq (find boxes r c c 'v') []

(* Test case for a real bug that happened with sample.txt

   ░░░░░░░░░░░░░░░░░░░░
   ░░□□··□□······□□□□░░
   ░░□□···········□□·░░
   ░░···········@□□□□░░ 3
   ░░··········▣▣·□□·░░ 4
   ░░··░░□□··□□·▣▣···░░ 5
   ░░···□□···□□··▣▣··░░ 6
   ░░·····□□··■■·▣▣□□░░ 7
   ░░········■■······░░ 8
   ░░░░░░░░░░░░░░░░░░░░
             111111
             012345

   Robot at (3, 13), moving down.
   Should only find ▣ boxes to move down.
   However, both ▣ and ■ boxes were found.
*)
let test_find_boxes_wide_vertical_bug _ =
  let assert_eq r e =
    assert_equal_wideposset (e |> WidePosSet.of_list) (r |> WidePosSet.of_list)
  in
  let find = find_boxes_wide_vertical in
  let r, c, move = (3, 13, 'v') in
  let boxes =
    [
      (1, 2, 3);
      (1, 6, 7);
      (1, 14, 15);
      (1, 16, 17);
      (2, 2, 3);
      (2, 15, 16);
      (3, 14, 15);
      (3, 16, 17);
      (4, 12, 13);
      (4, 15, 16);
      (5, 6, 7);
      (5, 10, 11);
      (5, 13, 14);
      (6, 5, 6);
      (6, 10, 11);
      (6, 14, 15);
      (7, 7, 8);
      (7, 11, 12);
      (7, 14, 15);
      (7, 16, 17);
      (8, 10, 11);
    ]
    |> WidePosSet.of_list
  in
  assert_eq (find boxes r c c move)
    [ (4, 12, 13); (5, 13, 14); (6, 14, 15); (7, 14, 15) ]

let test_move_boxes _ =
  let assert_eq r e = assert_equal e r ~printer:print_list_of_wide_pos in
  let boxes = [ (1, 2, 3); (1, 4, 5); (8, 7, 8) ] in
  assert_eq (move_boxes '<' boxes) [ (1, 1, 2); (1, 3, 4); (8, 6, 7) ];
  assert_eq (move_boxes '>' boxes) [ (1, 3, 4); (1, 5, 6); (8, 8, 9) ];
  assert_eq (move_boxes '^' boxes) [ (0, 2, 3); (0, 4, 5); (7, 7, 8) ];
  assert_eq (move_boxes 'v' boxes) [ (2, 2, 3); (2, 4, 5); (9, 7, 8) ]

let test_update_box_set _ =
  let assert_eq r e = assert_equal_posset (e |> PosSet.of_list) r in
  let boxes = [ (1, 2); (1, 3); (7, 8) ] |> PosSet.of_list in
  assert_eq (PosMod.update [] [] boxes) (boxes |> PosSet.to_list);
  assert_eq
    (PosMod.update [ (11, 22); (9, 9); (99, 99) ] [ (1, 2); (7, 8) ] boxes)
    [ (11, 22); (9, 9); (99, 99); (1, 3) ]

let test_update_wide_box_set _ =
  let assert_eq r e = assert_equal_wideposset (e |> WidePosSet.of_list) r in
  let boxes = [ (1, 2, 3); (1, 4, 5); (7, 7, 8) ] |> WidePosSet.of_list in
  assert_eq (WidePosMod.update [] [] boxes) (boxes |> WidePosSet.to_list);
  assert_eq
    (WidePosMod.update [ (11, 22, 33) ] [ (1, 2, 3) ] boxes)
    [ (11, 22, 33); (1, 4, 5); (7, 7, 8) ]

let test_collides _ =
  let walls = [ (1, 2); (1, 3); (2, 2) ] |> PosSet.of_list in
  let boxes_collide1 = [ (1, 2, 3); (7, 8, 9) ] |> WidePosSet.of_list in
  let boxes_collide2 = [ (1, 3, 4); (7, 8, 9) ] |> WidePosSet.of_list in
  let boxes_no_collide = [ (1, 4, 5); (7, 8, 9) ] |> WidePosSet.of_list in
  assert_bool "no boxes, no collision" (not (collides walls WidePosSet.empty));
  assert_bool "collision 1" (collides walls boxes_collide1);
  assert_bool "collision 2" (collides walls boxes_collide2);
  assert_bool "no collision" (not (collides walls boxes_no_collide))

let suite =
  "AoC Tests"
  >::: [
         "test_split_list" >:: test_split_list;
         "test_find_chars" >:: test_find_chars;
         "test_parse_lines" >:: test_parse_lines;
         "test_widen_walls" >:: test_widen_walls;
         "test_widen_boxes" >:: test_widen_boxes;
         "test_find_boxes" >:: test_find_boxes;
         "test_find_boxes_wide_horizontal" >:: test_find_boxes_wide_horizontal;
         "test_find_boxes_wide_vertical" >:: test_find_boxes_wide_vertical;
         "test_find_boxes_wide_vertical_bug"
         >:: test_find_boxes_wide_vertical_bug;
         "test_move_boxes" >:: test_move_boxes;
         "test_update_box_set" >:: test_update_box_set;
         "test_update_wide_box_set" >:: test_update_wide_box_set;
         "test_collides" >:: test_collides;
       ]

let () = run_test_tt_main suite
