type pos = int * int
type wide_pos = int * int * int

let ( ++ ) (a, b) (c, d) = (a + c, b + d)
let ( +++ ) (r, cl, cr) (dr, dc) = (r + dr, cl + dc, cr + dc)
let gps (r, c) = (100 * r) + c
let gps_wide (r, cl, _) = gps (r, cl)

module PosSet = Set.Make (struct
  type t = pos

  let compare = compare
end)

module WidePosSet = Set.Make (struct
  type t = wide_pos

  let compare = compare
end)

module PSet (S : Set.S) = struct
  let update to_add to_remove set =
    let to_add = S.of_list to_add in
    let to_remove = S.of_list to_remove in
    S.union (S.diff set to_remove) to_add
end

module PosMod = PSet (PosSet)
module WidePosMod = PSet (WidePosSet)

type input = { robot : pos; boxes : PosSet.t; walls : PosSet.t; moves : string }

type input_wide = {
  robot : pos;
  wide_boxes : WidePosSet.t;
  walls : PosSet.t;
  moves : string;
}

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

let widen_walls ps =
  Seq.fold_left
    (fun acc (r, c) -> PosSet.add (r, c * 2) (PosSet.add (r, (c * 2) + 1) acc))
    PosSet.empty
    PosSet.(to_seq ps)

let widen_boxes ps =
  Seq.fold_left
    (fun acc (r, c) -> WidePosSet.add (r, c * 2, (c * 2) + 1) acc)
    WidePosSet.empty
    PosSet.(to_seq ps)

let unwiden_boxes ps =
  Seq.fold_left
    (fun acc (r, cl, cr) -> PosSet.add (r, cl) (PosSet.add (r, cr) acc))
    PosSet.empty
    WidePosSet.(to_seq ps)

let print_grid robot boxes walls =
  let max_r, max_c =
    PosSet.fold
      (fun (r, c) (max_r, max_c) -> (max r max_r, max c max_c))
      walls (0, 0)
  in
  let to_string r c =
    if (r, c) = robot then "@"
    else if PosSet.mem (r, c) boxes then "▣"
    else if PosSet.mem (r, c) walls then "░"
    else "·"
  in
  let rec print_grid' r c =
    if r > max_r then ()
    else
      let () =
        if c > max_c then (
          print_newline ();
          print_grid' (r + 1) 0)
        else (
          to_string r c |> print_string;
          print_grid' r (c + 1))
      in
      ()
  in
  print_grid' 0 0

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

let rec find_boxes_wide_horizontal boxes (r, c) move =
  let _, dc = move_to_delta move in
  let cl, cr = (c + dc, c + (2 * dc)) in
  let cl, cr = (min cl cr, max cl cr) in
  let c_edge = if dc < 0 then cl else cr in
  match WidePosSet.mem (r, cl, cr) boxes with
  | false -> []
  | true -> (r, cl, cr) :: find_boxes_wide_horizontal boxes (r, c_edge) move

let rec find_boxes_wide_vertical boxes r left right move =
  let dr, _ = move_to_delta move in
  let boxes_in_row =
    WidePosSet.filter
      (fun (r', cl, cr) -> r' = r + dr && left - 1 <= cl && cr <= right + 1)
      boxes
    |> WidePosSet.to_list
  in
  match boxes_in_row with
  | [] -> []
  | _ ->
      let cs =
        List.concat (List.map (fun (_, cl, cr) -> [ cl; cr ]) boxes_in_row)
      in
      let left = List.fold_left min (List.hd cs) (List.tl cs) in
      let right = List.fold_left max (List.hd cs) (List.tl cs) in
      boxes_in_row @ find_boxes_wide_vertical boxes (r + dr) left right move

let move_boxes move = List.map (fun (r, c) -> (r, c) ++ move_to_delta move)

let move_boxes_wide move =
  List.map (fun (r, cl, cr) -> (r, cl, cr) +++ move_to_delta move)

let collides walls boxes =
  WidePosSet.exists
    (fun (r, cl, cr) -> PosSet.mem (r, cl) walls || PosSet.mem (r, cr) walls)
    boxes

let debug_part_1 = false

let simulate { robot; boxes; walls; moves } =
  let rec simulate' robot boxes moves =
    if debug_part_1 then print_grid robot boxes walls;
    match moves with
    | [] -> boxes |> PosSet.to_seq
    | move :: moves_rest ->
        if debug_part_1 then Printf.printf "\nMove: %c\n" move;
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
              let moved_boxes = move_boxes move boxes_to_move in
              let new_boxes = PosMod.update moved_boxes boxes_to_move boxes in
              simulate' moved_robot new_boxes moves_rest
  in
  simulate' robot boxes (String.to_seq moves |> List.of_seq)

let debug_part_2 = false

let simulate_wide { robot; wide_boxes; walls; moves } =
  let rec simulate_wide' robot wide_boxes moves =
    if debug_part_2 then print_grid robot (unwiden_boxes wide_boxes) walls;
    match moves with
    | [] -> wide_boxes |> WidePosSet.to_seq
    | move :: moves_rest ->
        if debug_part_2 then Printf.printf "\nMove: %c\n" move;
        let moved_robot = robot ++ move_to_delta move in
        if PosSet.mem moved_robot walls then
          simulate_wide' robot wide_boxes moves_rest
        else if move = '<' || move = '>' then
          (* horizontal move *)
          let boxes_to_move =
            find_boxes_wide_horizontal wide_boxes robot move
          in
          if boxes_to_move = [] then
            simulate_wide' moved_robot wide_boxes moves_rest
          else
            let _, dc = move_to_delta move in
            let lr, lcl, lcr = List.hd (List.rev boxes_to_move) in
            let behind_last_box =
              if move = '<' then (lr, lcl + dc) else (lr, lcr + dc)
            in
            if PosSet.mem behind_last_box walls then
              simulate_wide' robot wide_boxes moves_rest
            else
              let moved_boxes = move_boxes_wide move boxes_to_move in
              let new_boxes =
                WidePosMod.update moved_boxes boxes_to_move wide_boxes
              in
              simulate_wide' moved_robot new_boxes moves_rest
        else
          (* vertical move *)
          let r, c = robot in
          let boxes_to_move = find_boxes_wide_vertical wide_boxes r c c move in
          if boxes_to_move = [] then
            simulate_wide' moved_robot wide_boxes moves_rest
          else
            let moved_boxes = move_boxes_wide move boxes_to_move in
            let collision = collides walls (WidePosSet.of_list moved_boxes) in
            if collision then simulate_wide' robot wide_boxes moves_rest
            else
              let new_boxes =
                WidePosMod.update moved_boxes boxes_to_move wide_boxes
              in
              simulate_wide' moved_robot new_boxes moves_rest
  in
  simulate_wide' robot wide_boxes (String.to_seq moves |> List.of_seq)

let part1 input = simulate input |> Seq.map gps |> Seq.fold_left ( + ) 0

let part2 { robot; boxes; walls; moves } =
  let rr, rc = robot in
  let robot = (rr, rc * 2) in
  let wide_boxes = widen_boxes boxes in
  let walls = widen_walls walls in
  let boxes = simulate_wide { robot; wide_boxes; walls; moves } in
  boxes |> Seq.map gps_wide |> Seq.fold_left ( + ) 0
