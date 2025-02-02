type pos = int * int

module PosSet = Set.Make (struct
  type t = pos

  let compare = compare
end)

type input = { robot : pos; boxes : PosSet.t; walls : PosSet.t; moves : string }

let rec split_list elem = function
  | [] -> ([], [])
  | hd :: tl ->
      if hd = elem then ([], tl)
      else
        let before, after = split_list elem tl in
        (hd :: before, after)

let find_chars rows char =
  let rec find_chars' rows char r res =
    match rows with
    | [] -> res
    | row :: rows_rest ->
        let ws = String.to_seqi row |> Seq.filter (fun (_, c) -> c = char) in
        let ws = Seq.map (fun (c, _) -> (r, c)) ws in
        let ws = PosSet.of_seq ws in
        find_chars' rows_rest char (r + 1) (PosSet.union res ws)
  in
  find_chars' rows char 0 PosSet.empty

let parse_lines lines =
  let grid, moves = split_list "" lines in
  let boxes = find_chars grid 'O' in
  let walls = find_chars grid '#' in
  let robot = find_chars grid '@' |> PosSet.choose_opt |> Option.get in
  { robot; boxes; walls; moves = String.concat "" moves }

let print_grid robot boxes walls =
  let max_r, max_c =
    PosSet.fold
      (fun (r, c) (max_r, max_c) -> (max r max_r, max c max_c))
      walls (0, 0)
  in
  let to_char r c =
    if PosSet.mem (r, c) walls then '#'
    else if PosSet.mem (r, c) boxes then 'O'
    else if (r, c) = robot then '@'
    else '.'
  in
  let rec print_grid' r c =
    if r > max_r then ()
    else
      let () =
        if c > max_c then (
          print_newline ();
          print_grid' (r + 1) 0)
        else (
          to_char r c |> print_char;
          print_grid' r (c + 1))
      in
      ()
  in
  print_grid' 0 0

let ( ++ ) (a, b) (c, d) = (a + c, b + d)

let move_to_delta = function
  | '<' -> (0, -1)
  | '>' -> (0, 1)
  | '^' -> (-1, 0)
  | 'v' -> (1, 0)
  | _ -> failwith "Invalid move"

let rec find_boxes boxes pos move =
  let pos' = pos ++ move_to_delta move in
  match PosSet.mem pos' boxes with
  | false -> []
  | true -> pos' :: find_boxes boxes pos' move

let move_boxes boxes boxes_to_move move =
  let move_set = PosSet.of_list boxes_to_move in
  let remaining = PosSet.diff boxes move_set in
  PosSet.fold
    (fun box boxes' -> PosSet.add (box ++ move_to_delta move) boxes')
    move_set remaining

let debug = false

let simulate { robot; boxes; walls; moves } =
  let rec simulate' robot boxes moves =
    if debug then print_grid robot boxes walls;
    match moves with
    | [] -> boxes |> PosSet.to_seq
    | move :: moves_rest ->
        if debug then Printf.printf "\nMove: %c\n" move;
        let moved_robot = robot ++ move_to_delta move in
        if PosSet.mem moved_robot walls then simulate' robot boxes moves_rest
        else
          let boxes_to_move = find_boxes boxes robot move in
          if boxes_to_move = [] then simulate' moved_robot boxes moves_rest
          else
            let last_box = List.hd (List.rev boxes_to_move) in
            let behind_last_box = last_box ++ move_to_delta move in
            if PosSet.mem behind_last_box walls then
              simulate' robot boxes moves_rest
            else
              let new_boxes = move_boxes boxes boxes_to_move move in
              simulate' moved_robot new_boxes moves_rest
  in
  simulate' robot boxes (String.to_seq moves |> List.of_seq)

let gps (r, c) = (100 * r) + c
let part1 input = simulate input |> Seq.map gps |> Seq.fold_left ( + ) 0
