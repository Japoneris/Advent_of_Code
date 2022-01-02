let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    nest (line::lst)
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    (List.rev lst)
  in nest []
  ;;

let find_row stri =
  let rec nest v0 v1 i =
  if i == 7 then v0
  else (
    let mid = (v0 + v1) / 2 in
    if stri.[i] == 'F'
    then nest v0 mid (i+1)
    else nest mid v1 (i+1)
    ) in nest 0 128 0
  ;;

let find_col stri =
  let rec nest v0 v1 i =
  if i == 10 then v0
  else (
    let mid = (v0 + v1) / 2 in
    if stri.[i] == 'L'
    then nest v0 mid (i+1)
    else nest mid v1 (i+1)
    ) in nest 0 8 7
  ;;

let boards = read_file "input_05";;
let boards_rc = List.map (fun x ->
  let ri = find_row x
  and ci = find_col x in
  (x, ri, ci, ri*8 + ci)
  ) boards;;

let (pass_max, r_max, c_max, id_max) =  List.fold_left (fun (s, r, c, v) (si, ri, ci, vi) ->
    if vi > v
    then (si, ri, ci, vi)
    else (s, r, c, v)
  ) ("", 0,0,0) boards_rc;;

  Printf.printf "Pass %s at pos %d:%d with ID %d\n" pass_max r_max c_max id_max;;

let ids = List.map (fun (s, r, c, v) -> v) boards_rc;;
let ids_sort = List.sort (fun a b -> a - b) ids;;

List.fold_left (fun mem x ->
  if x - mem > 1
  then Printf.printf "%d::%d ==> ID=%d, " mem x (x+mem)/2;
  x
  ) 0 ids_sort;;
