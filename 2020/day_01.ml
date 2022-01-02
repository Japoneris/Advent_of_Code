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


let data = read_file "AOC_2020/input_01";;
let numb = List.map (fun x -> int_of_string x) data ;;

let search_num lst n =
  let rec nest ni = function
  | [] -> (0, 0)
  | nj::lj ->
    if ni + nj == n
    then (ni, nj)
    else nest ni lj
  in

  let rec nest2 mem = function
  | [] -> mem
  | x::li -> nest2 ((nest x li)::mem) li
  in
  nest2 [] lst
;;


let listing = search_num numb 2020;;
List.iter (fun (a, b) ->
  if a + b == 2020
  then Printf.printf "%d x %d = %d" a b (a*b)) listing;;

(**)

let search_two_num lst n =
  let rec nest a = function
  | [] -> (0, 0)
  | b::li ->
    if a + b == n then (a, b)
    else nest a li
  in

  let (result, _) = List.fold_left (fun (tup, mem) x ->
    let (i, j) = nest x mem in
    if i + j == 0 then (tup, x::mem) else ((i, j), x::mem)
    ) ((0, 0), []) lst
  in result
;;

let search_three_num lst n =
  (*Increasing order*)

  let ((a, b, c), _) = List.fold_left (fun (tup, mem) x -> 
    let (i, j) = search_two_num mem (n - x) in
    if i + j + x == n
    then ((i, j, x), x::mem)
    else (tup, x::mem)
  ) ((0, 0, 0), []) lst
  in (a, b, c)
;;

let l1 = List.sort (fun a b -> a - b) lst in
  let rec nest = function
  | a::b::c::lst ->
    if a + b + c > n then []
    else if a + b + c == n then [(a, b, c)]
    else (nest (a::c::lst))
  | _ -> []
