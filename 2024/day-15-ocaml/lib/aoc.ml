let ( >> ) f g x = g (f x)
let ( ++ ) (a, b) (c, d) = (a + c, b + d)
let ( +++ ) (r, cl, cr) (dr, dc) = (r + dr, cl + dc, cr + dc)

type pos = int * int
type wide_pos = int * int * int

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

type input = {
  robot : pos;
  boxes : WidePosSet.t;
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
  let boxes =
    find_chars grid 'O' |> PosSet.to_seq
    |> Seq.map (fun (r, c) -> (r, c, c))
    |> WidePosSet.of_seq
  in
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
    (fun acc (r, c, _) -> WidePosSet.add (r, c * 2, (c * 2) + 1) acc)
    WidePosSet.empty
    WidePosSet.(to_seq ps)

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
  let r, c = pos ++ move_to_delta move in
  let box = (r, c, c) in
  match WidePosSet.mem box boxes with
  | false -> []
  | true -> box :: find_boxes boxes (r, c) move

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

let find_boxes_wide boxes pos move =
  if move = '<' || move = '>' then find_boxes_wide_horizontal boxes pos move
  else find_boxes_wide_vertical boxes (fst pos) (snd pos) (snd pos) move

let move_boxes move =
  List.map (fun (r, cl, cr) -> (r, cl, cr) +++ move_to_delta move)

let collides walls boxes =
  WidePosSet.exists
    (fun (r, cl, cr) -> PosSet.mem (r, cl) walls || PosSet.mem (r, cr) walls)
    boxes

let debug = false

let simulate find_boxes_fn { robot; boxes; walls; moves } =
  let rec sim robot boxes moves =
    if debug then print_grid robot (unwiden_boxes boxes) walls;
    match moves with
    | [] -> boxes
    | move :: moves_rest ->
        if debug then Printf.printf "\nMove: %c\n" move;
        let moved_robot = robot ++ move_to_delta move in
        if PosSet.mem moved_robot walls then sim robot boxes moves_rest
        else
          let boxes_to_move = find_boxes_fn boxes robot move in
          if boxes_to_move = [] then sim moved_robot boxes moves_rest
          else
            let moved_boxes = move_boxes move boxes_to_move in
            if collides walls (WidePosSet.of_list moved_boxes) then
              sim robot boxes moves_rest
            else
              let moved_boxes = move_boxes move boxes_to_move in
              let new_boxes =
                WidePosMod.update moved_boxes boxes_to_move boxes
              in
              sim moved_robot new_boxes moves_rest
  in
  sim robot boxes (String.to_seq moves |> List.of_seq)

let widen_input { robot; boxes; walls; moves } =
  let robot = (fst robot, snd robot * 2) in
  let boxes = widen_boxes boxes in
  let walls = widen_walls walls in
  { robot; boxes; walls; moves }

let gps (r, cl, _) = (100 * r) + cl

let part_n find_boxes_fn =
  simulate find_boxes_fn >> WidePosSet.to_seq >> Seq.map gps
  >> Seq.fold_left ( + ) 0

let part1 = part_n find_boxes
let part2 = widen_input >> part_n find_boxes_wide
