let parse_line line =
  let lst = String.split_on_char ',' line in
  List.map (int_of_string) lst
;;

let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    parse_line line
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    (List.rev lst)
  in nest []
  ;;

let lst = read_file "input_07.txt";;

let get_min_fuel_position lst =
  let maxi = List.fold_left (max) 0 lst in

  let arr = Array.init maxi (fun i ->
    (i, List.fold_left (fun mem x -> mem + (abs (i - x))) 0 lst)
    ) in
  Array.fold_left (fun (i, mem) (j, x) -> if x < mem then (j, x) else (i, mem)) (0, 100000000) arr
;;

let a1 = get_min_fuel_position lst;;
Printf.printf "Fuel part 1: %d\n" a1;;

let get_min_fuel_position_v2 lst =
  let maxi = List.fold_left (max) 0 lst in
  let cost = Array.init (maxi+1) (fun i ->  (i * (i+1)) / 2) in

  let arr = Array.init maxi (fun i ->
    (i, List.fold_left (fun mem x -> mem + cost.(abs (i - x))) 0 lst)
    ) in
  Array.fold_left (fun (i, mem) (j, x) -> if x < mem then (j, x) else (i, mem)) (0, 100000000) arr
;;

let a2 = get_min_fuel_position_v2 lst;;
Printf.printf "Fuel part 2: %d\n" a1;;
