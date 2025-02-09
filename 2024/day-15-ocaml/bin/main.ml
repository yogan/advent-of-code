open Aoc

let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let run_and_print fn input = fn input |> string_of_int |> print_endline

let () =
  let input = read_file Sys.argv.(1) |> parse_lines in
  run_and_print part1 input;
  run_and_print part2 input
