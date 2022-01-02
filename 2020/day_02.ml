let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    nest (line::lst)
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    lst
  in nest []
  ;;


let myline = "3-5 m: mhmfbq";;


let transform_input line =
  Scanf.sscanf line "%d-%d %c: %s" (fun a b c d -> (a, b, c, d)) ;;


let string_to_char_list s = Array.init (String.length s) (String.get s)
;;

let count_char c stri =


let is_valid (i0, i1, c, stri) =
  let a1 = string_to_char_list stri in
  let c1 = Array.fold_left (fun mem x ->
    if x == c
    then mem+1
    else mem) 0 a1 in
  (i0 <= c1) && (c1 <= i1)
;;


let my_lines =  read_file "AOC_2020/input_02";;

List.fold_left (fun mem x ->
  let tup = transform_input x in
  if is_valid tup
  then mem+1
  else mem

  ) 0 my_lines;;


let is_valid_v2 (i0, i1, c, stri) =
  let a1 = string_to_char_list stri in
  ((a1.(i0-1) == c) && (a1.(i1-1) != c)) || ((a1.(i0-1) != c) && (a1.(i1-1) == c))
;;

List.fold_left (fun mem x ->
  let tup = transform_input x in
  if is_valid_v2 tup
  then mem+1
  else mem

  ) 0 my_lines;;
