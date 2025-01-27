let parse_lines =
  List.map (fun line -> String.split_on_char 'x' line |> List.map int_of_string)

let volume = List.fold_left ( * ) 1

let surface_area lst =
  let l, w, h =
    match lst with [ l; w; h ] -> (l, w, h) | _ -> failwith "invalid input"
  in
  (2 * l * w) + (2 * w * h) + (2 * h * l)

let sum fn = List.fold_left (fun acc x -> acc + fn x) 0
let part1 = sum volume
let part2 = sum surface_area
