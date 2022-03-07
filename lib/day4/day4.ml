let split_to_grids (height : int) (lines : int list list) =
  let rec _split_to_grids
      lines
      (index : int)
      (max_index : int)
      (grid : int list list)
      (grids : int list list list)
    =
    if index = max_index
    then _split_to_grids lines 0 max_index [] (List.cons (List.rev grid) grids)
    else (
      match lines with
      | h :: t -> _split_to_grids t (index + 1) max_index (List.cons h grid) grids
      | [] -> List.rev grids)
  in
  _split_to_grids lines 0 height [] []
;;

let print_grid (grids : int list list) =
  print_endline "";
  List.iter
    (fun line ->
      List.iter (fun num -> Printf.printf "%i;" num) line;
      print_endline "")
    grids;
  print_endline ""
;;

let check_horizontals (numbers : int list) (grid : int list list) =
  let rec _check_horizontals (numbers : int list) (grid : int list list) =
    match grid with
    | h :: t ->
      let finded_nums =
        List.filter (fun num -> List.mem num numbers) h
        |> List.fold_left
             (fun nums num -> if List.mem num nums then nums else num :: nums)
             []
      in
      if List.length finded_nums = 5
      then Some finded_nums
      else _check_horizontals numbers t
    | [] -> None
  in
  _check_horizontals numbers grid
;;

let check_verticals (numbers : int list) (grid : int list list) =
  let rec _check_verticals
      (numbers : int list)
      (column_index : int)
      (grid : int list list)
    =
    if List.length grid = column_index
    then None
    else (
      let finded_nums =
        List.map (fun line -> List.nth line column_index) grid
        |> List.filter (fun num -> List.mem num numbers)
        |> List.fold_left
             (fun nums num -> if List.mem num nums then nums else num :: nums)
             []
      in
      if List.length finded_nums = 5
      then Some finded_nums
      else _check_verticals numbers (column_index + 1) grid)
  in
  _check_verticals numbers 0 grid
;;

let check_numbers (numbers : int list) (grids : int list list list) =
  let _check_numbers (numbers_to_check : int list) (grid : int list list) =
    let verticals_matched = check_verticals numbers_to_check grid in
    if Option.is_some verticals_matched
    then verticals_matched
    else (
      let horizontal_matched = check_horizontals numbers_to_check grid in
      if Option.is_some horizontal_matched then horizontal_matched else None)
  in
  let rec iter_grids grids (numbers_to_check : int list) =
    match grids with
    | hd :: tl ->
      (match _check_numbers numbers_to_check hd with
      | Some _ -> Some hd
      | None -> iter_grids tl numbers_to_check)
    | [] -> None
  in
  let rec iter_numbers (head_list : int list) (tail_list : int list) =
    match tail_list with
    | h :: t ->
      let finded_nums = iter_grids grids head_list in
      (match finded_nums with
      | Some grid ->
        List.filter (fun v -> not (List.mem v head_list)) (List.concat grid)
          |> List.fold_left ( + ) 0
          |> ( * ) (List.nth head_list 0)
      | _ -> iter_numbers (h :: head_list) t)
    | [] -> 0
  in
  iter_numbers
    (List.filteri (fun i _ -> i < 5) numbers)
    (List.filteri (fun i _ -> i >= 5) numbers)
;;

let test1 path =
  let input_data_channel = open_in path in
  let lines = Utils.read_all_lines input_data_channel in
  let lines = List.filter (fun line -> line <> "") lines in
  let lines_grids = List.filteri (fun i _ -> i != 0) lines in
  let nums = List.map(fun v -> int_of_string v) (Str.split (Str.regexp ",") (List.nth lines 0))  in
  let lines_grids =
    List.map
      (fun line ->
        Str.split (Str.regexp " ") line
        |> List.filter (fun num -> num <> "")
        |> List.map int_of_string)
      lines_grids
  in
  let result = split_to_grids 5 lines_grids
  |> check_numbers nums in
  assert (result = 46920)
;;
