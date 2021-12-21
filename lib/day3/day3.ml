let lines_to_bits (elems : string list) : string list list =
  List.fold_left
    (fun (lines : string list list) (line : string) ->
      (List.fold_left
         (fun (ln : string list) (chr : string) -> chr :: ln)
         []
         (Str.split (Str.regexp "") line)
      |> List.rev)
      :: lines)
    []
    elems
  |> List.rev
;;

let columns_to_rows (elems : string list list) : string list list =
  let columns_to_rows_ (elems : string list list) (index : int) : string list =
    List.fold_left (fun column line -> List.nth line index :: column) [] elems |> List.rev
  in
  let rec loop count end_range res =
    match count with
    | x when x == end_range -> res
    | _ -> loop (count + 1) end_range (columns_to_rows_ elems count :: res)
  in
  loop 0 (List.length (List.nth elems 0)) []
;;

let must_common_elements (columns : string list list) (val_if_eq : string) (rev : bool)
    : string list
  =
  let _must_common_elements_in_column (res : string list) column =
    let zero_list, one_list =
      ( List.length (List.filter (String.equal "0") column)
      , List.length (List.filter (String.equal "1") column) )
    in
    let value =
      if zero_list > one_list
      then (if rev then "1" else "0") :: res
      else if zero_list < one_list
      then (if rev then "0" else "1") :: res
      else val_if_eq :: res
    in
    value
  in
  List.fold_left _must_common_elements_in_column [] columns
;;

let reverse_bits (columns : string list) : string list =
  List.fold_left
    (fun res i -> if String.equal i "0" then "1" :: res else "0" :: res)
    []
    columns
  |> List.rev
;;

type rating_type =
  | Oxygen
  | CO2

let must_common_elements_filter (columns : string list list) (rating : rating_type)
    : string list
  =
  let _filter (position : int) (value : string) (elements : string list list) =
    List.filter (fun v -> String.equal (List.nth v position) value) elements
  in
  let if_eq, common_rev =
    match rating with
    | Oxygen -> "1", false
    | CO2 -> "0", true
  in
  let rec _must_common_elements_filter (columns : string list list) (index : int) =
    let common_elements =
      must_common_elements (columns |> columns_to_rows) if_eq common_rev
    in
    let common_element = List.nth common_elements index in
    let remaind = _filter index common_element columns in
    match remaind with
    | [ h ] -> h
    | _ :: _ -> _must_common_elements_filter remaind (index + 1)
    | _ -> failwith "empty list"
  in
  _must_common_elements_filter columns 0
;;

let join_bits_to_int (bits : string list) : int =
  bits |> List.fold_left ( ^ ) "" |> Printf.sprintf "0b%s" |> int_of_string
;;

let test1 path =
  let input_data_channel = open_in path in
  let lines = Utils.read_all_lines input_data_channel in
  let gamma = must_common_elements (lines_to_bits lines |> columns_to_rows) "1" false in
  let epsilon = reverse_bits gamma in
  assert (join_bits_to_int gamma * join_bits_to_int epsilon == 4174964)
;;

let test2 path =
  let input_data_channel = open_in path in
  let lines = Utils.read_all_lines input_data_channel |> lines_to_bits in
  assert (
    (must_common_elements_filter lines Oxygen |> join_bits_to_int)
    * (must_common_elements_filter lines CO2 |> join_bits_to_int)
    == 4474944)
;;
