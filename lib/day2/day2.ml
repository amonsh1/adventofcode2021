let count_increases (elems : int list) : int =
  let rec _count_increases (elems : int list) (count : int) : int =
    match elems with
    | h :: next :: t ->
      if next > h
      then _count_increases (next :: t) (count + 1)
      else _count_increases (next :: t) count
    | _ -> count
  in
  _count_increases elems 0
;;

let run_commands (commands : (string * int) list) =
  let _run_commands (command : string * int) (x : int) (y : int) : int * int =
    match command with
    | "forward", num -> x + num, y
    | "down", num -> x, y + num
    | "up", num -> x, y - num
    | _ -> x, y
  in
  List.fold_left
    (fun res command -> _run_commands command (fst res) (snd res))
    (0, 0)
    commands
;;

let run_commands2 (commands : (string * int) list) =
  let _run_commands (command : string * int) ((x, y, aim) : int * int * int)
      : int * int * int
    =
    match command with
    | "forward", num -> x + num, y + (aim * num), aim
    | "down", num -> x, y, aim + num
    | "up", num -> x, y, aim - num
    | _ -> x, y, aim
  in
  List.fold_left (fun res command -> _run_commands command res) (0, 0, 0) commands
;;

let test1 path =
  let input_data_channel = open_in path in
  let lines = Utils.read_all_lines input_data_channel in
  let x, y =
    run_commands
      (List.map
         (fun line ->
           Str.split (Str.regexp " ") line
           |> fun lst -> List.nth lst 0, int_of_string (List.nth lst 1))
         lines)
  in
  assert (x * y == 1882980)
;;

let test2 path =
  let input_data_channel = open_in path in
  let lines = Utils.read_all_lines input_data_channel in
  let x, y, _ =
    run_commands2
      (List.map
         (fun line ->
           Str.split (Str.regexp " ") line
           |> fun lst -> List.nth lst 0, int_of_string (List.nth lst 1))
         lines)
  in
  assert (x * y == 1971232560)
;;
