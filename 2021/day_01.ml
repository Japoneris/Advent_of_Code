let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    nest ((int_of_string line)::lst)
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    (List.rev lst)
  in nest []
  ;;

let count_increase lst =
  let rec nest cnt = function
  | [] -> cnt
  | x::[] -> cnt
  | x::y::lst -> if x < y
  then nest (cnt+1) (y::lst)
  else nest cnt (y::lst)
  in nest 0 lst
  ;;

let count_sliding_increase lst =
  let rec nest cnt va = function
  | [] -> cnt
  | x::[] -> cnt
  | x::y::[] -> cnt
  | x::y::z::lst ->
  let vb = x + y + z in
  if vb > va
  then nest (cnt+1) vb (y::z::lst)
  else nest cnt vb (y::z::lst)
  in nest (-1) 0 lst
  ;;

let lst = read_file "input_01.txt";;

let res = count_increase lst;;
Printf.printf "Number of increase: %d" res;;

let res = count_sliding_increase lst;;
Printf.printf "Number of sliding increase: %d" res;;
