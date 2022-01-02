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


let data = read_file "input_09";;
let numbers = List.map (fun line ->
     Scanf.sscanf line "%d " (fun x -> x)
     ) data;;

let split_list lst n =
  let rec nest i li lj =
  if i == n then (List.rev li, lj)
  else match lj with
  | [] -> (List.rev li, [])
  | x::lx -> nest (i+1) (x::li) lx
  in nest 0 [] lst
;;

let search_sum lst v =
  let rec nest = function
  | [] -> false
  |  y::ly ->
    if (List.fold_left (fun mem x ->
      if x + y == v then true
      else mem
      ) false ly)
    then true
    else nest ly
  in nest lst
;;



let solve_p1 numbers n =
  let (p1, p2) = split_list numbers n in

  let rec nest pre = function
  | [] -> 0
  | x::lx -> (
    if search_sum pre x
    then nest (List.tl (pre@[x])) lx
    else (Printf.printf "%d is wrong" x; x)
    ) in nest p1 p2
  ;;

let v1 = 20874512;;

let get_mini lst =
  List.fold_left (fun mem x -> min mem x) 10000000 lst
  ;;

let get_maxi lst =
  List.fold_left (fun mem x -> max mem x) 0 lst
  ;;

let search_contiguous v lst =
  let rec nest vtot mem = function
  | [] -> (false, 0)
  | x::lx -> (
    if vtot + x == v
    then (true, get_maxi (x::mem) + get_mini (x::mem) )
    else if vtot + x > v
    then (false, 0)
    else nest (vtot+x) (x::mem) lx
    ) in nest 0 [] lst
  ;;


  let solve_p2 number v =
    let rec nest = function
    | [] -> (false, 0)
    | x::lx -> (
      let (tf, v1) = search_contiguous v (x::lx) in
      if tf
      then (true, v1)
      else nest lx
      ) in nest number
    ;;
