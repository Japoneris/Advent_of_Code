let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    nest (line::lst)
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    Array.of_list (List.rev lst)
  in nest []
  ;;

let pattern = read_file "AOC_2020/input_03";;
let l0 = Array.length pattern;;
let l1 = String.length pattern.(0);;

let count_tree dr dc patt =
  let l0 = Array.length patt in
  let l1 = String.length patt.(0) in

  let rec nest cnt pr pc =
  if pr >= l0 then cnt
  else nest (cnt + if patt.(pr).[pc mod l1] == '#' then 1 else 0) (pr+dr) (pc + dc)
  in nest 0 0 0
;;


let answer1 = count_tree 1 3 pattern;;
Printf.printf "Start 1: %d\n" answer1;;

let a1 = count_tree 1 1 pattern
and a2 = count_tree 1 3 pattern
and a3 = count_tree 1 5 pattern
and a4 = count_tree 1 7 pattern
and a5 = count_tree 2 1 pattern
in

Printf.printf "%d x %d x %d x %d x %d = %d" a1 a2 a3 a4 a5 (a1*a2*a3*a4*a5);;
