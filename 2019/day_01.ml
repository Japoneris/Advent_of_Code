
let scanint x = Scanf.sscanf x "%d " (fun y -> y);;

let read_line ic =
  let v = scanint (input_line ic) in
  Printf.printf "%d, " v;
  (v / 3) - 2
  ;;

let transformy v =
  v
;;

let transformx v =
  let rec nest tot vi =
    let vn = vi / 3 - 2 in
    if vn <= 0
    then tot
    else nest (tot + vn) vn
  in nest 0 v
;;

let read_line ic transformer =
  let v = transform (input_line ic) in
  transformer v
;;



let read_network transformer =
  let ic = open_in "input_01" in

  let rec nest tot =
  try
    nest (tot + (read_line ic transformer))
   with e ->
   close_in_noerr ic;
   tot in
  nest 0
;;

let result_1 = read_network transformy;;
let result_2 = read_network transformx;;

Printf.printf "Result for part 1: \t%d\n" result_1;
Printf.printf "Result for part 2: \t%d\n" result_2;
