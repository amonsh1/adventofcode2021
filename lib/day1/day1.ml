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

let count_windows_increases (elems : int list) : int =
  let rec _count_windows_increases
      (elems : int list)
      (prev_a : int)
      (prev_b : int)
      (prev_c : int)
      (count : int)
      : int
    =
    match elems with
    | cur_a :: cur_b :: cur_c :: t ->
      if cur_a + cur_b + cur_c > prev_a + prev_b + prev_c
      then _count_windows_increases (cur_b :: cur_c :: t) cur_a cur_b cur_c (count + 1)
      else _count_windows_increases (cur_b :: cur_c :: t) cur_a cur_b cur_c count
    | _ -> count
  in
  match elems with
  | a :: b :: c :: t -> _count_windows_increases (b :: c :: t) a b c 0
  | _ -> 0
;;

let test2 (path: string) =
  let input_data_channel =
    open_in
    path
  in
  let lines = List.map int_of_string (Utils.read_all_lines input_data_channel) in
  let c = count_windows_increases lines in
  assert (c == 1797)
;;

let test1 (path: string) =
  let input_data_channel =
    open_in
    path
  in

  let lines = List.map int_of_string (Utils.read_all_lines input_data_channel) in
  let c = count_increases lines in
  assert (c == 1766)
;;

