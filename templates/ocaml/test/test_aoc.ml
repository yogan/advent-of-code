open OUnit2
open Aoc

let string_of_list fn lst = "[" ^ String.concat "; " (List.map fn lst) ^ "]"
let string_of_list_of_int = string_of_list string_of_int
let string_of_list_of_list = string_of_list string_of_list_of_int

let test_parse_lines _ =
  assert_equal [] (parse_lines []) ~printer:string_of_list_of_list;
  assert_equal
    [ [ 1; 2; 3 ]; [ 4; 5; 66 ] ]
    (parse_lines [ "1x2x3"; "4x5x66" ])
    ~printer:string_of_list_of_list

let test_volume _ =
  assert_equal 6 (volume [ 1; 2; 3 ]) ~printer:string_of_int;
  assert_equal 1320 (volume [ 4; 5; 66 ]) ~printer:string_of_int

let test_surface_area _ =
  assert_equal 6 (surface_area [ 1; 1; 1 ]) ~printer:string_of_int;
  assert_equal (4 + 6 + 12) (surface_area [ 1; 2; 3 ]) ~printer:string_of_int

let test_surface_area_failure _ =
  assert_raises (Failure "invalid input") (fun () -> surface_area [ 1; 2 ]);
  assert_raises (Failure "invalid input") (fun () ->
      surface_area [ 1; 2; 3; 4 ])

let suite =
  "AoC Tests"
  >::: [
         "test_parse_lines" >:: test_parse_lines;
         "test_volume" >:: test_volume;
         "test_surface_area" >:: test_surface_area;
         "test_surface_area_failure" >:: test_surface_area_failure;
       ]

let () = run_test_tt_main suite
