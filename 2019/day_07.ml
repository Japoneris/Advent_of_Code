let decompose data i =
  if data.(i) == 99
  then (99, 0, 0, 0)
  else if data.(i) == 4
  then (4, data.(data.(i+1)), 0, 0)
  else (
    let (op, inter) = (data.(i) mod 100, data.(i) / 100) in
    if (op <= 2) || (op > 4)
    then (
      let a = if inter mod 10 == 0 then data.(data.(i+1)) else data.(i+1)
      and b = if (inter / 10) mod 10 == 0 then data.(data.(i+2)) else data.(i+2)
      and c = data.(i+3) in
      (op, a, b, c)
      )
      else (op, data.(i+1), 0, 0)
    )
;;

let machine_state data inp =
  let config = ref (-1) in

  let process_code op a b c i =
  if op == 1
  then (data.(c) <- a + b; i + 4)
  else if op == 2
  then (data.(c) <- a * b; i + 4)
  else if op == 3
  then (data.(a) <- inp; i + 2)
  else if op == 4
  then (
    Printf.printf "DIAG: %d\n" a;
    config := a mod 10;
    if a == 0
    then i+2
    else i+2 (*100000*)
    (*i + 2*)
    )
  else if op == 5
  then (if a != 0 then b else i + 3 )
  else if op == 6
  then (if a == 0 then b else i+3)
  else if op == 7
  then (data.(c) <- if a < b then  1 else 0; i+4 )
  else if op == 8
  then (data.(c) <- if a == b then  1 else 0; i+4)
  else if op == 99
  then (Printf.printf "Halting wih code 99\n"; i + 10000)
  else (Printf.printf "Error ! with code %d\n" op; i + 10000)
  in

  let l = Array.length data in

  let rec nest i =
    if i >= l then !config
    else (
      Printf.printf "%d:" i;
      let (op, a, b, c) = decompose data i in
      Printf.printf "\t%d (%d %d %d)\n" op a b c;
      let ip = process_code op a b c i in
      nest ip
      )
    in nest 0
;;

let silent_machine_state data inp =
  let config = ref (-1) in
  let l = Array.length data in

  let process_code op a b c i =
  if op == 1
  then (data.(c) <- a + b; i + 4)
  else if op == 2
  then (data.(c) <- a * b; i + 4)
  else if op == 3
  then (data.(a) <- inp; i + 2)
  else if op == 4
  then (config := a mod 10; (*i + 2*)
  if a == 0
  then i+2 else 100000)
  else if op == 5
  then (if a != 0 then b else i + 3 )
  else if op == 6
  then (if a == 0 then b else i+3)
  else if op == 7
  then (data.(c) <- if a < b then  1 else 0; i+4 )
  else if op == 8
  then (data.(c) <- if a == b then  1 else 0; i+4)
  else if op == 99
  then (i + 10000)
  else (Printf.printf "Error ! with code %d\n" op; i + 10000)
  in

  let rec nest i =
    if i >= l then !config
    else (
      let (op, a, b, c) = decompose data i in
      let ip = process_code op a b c i in
      nest ip)
    in nest 0
;;

let scanint x = Scanf.sscanf x "%d " (fun y -> y);;
let transform x = Array.of_list (List.map (scanint) (String.split_on_char ',' x));;

let read_input_data path =
  let ic = open_in path in
  let stri  = input_line ic in
  close_in_noerr ic;
  stri
;;

let test_0 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0";;
let test_1 = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0";;
let test_2 = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
let real = read_input_data "input_07";;
let past = read_input_data "input_05";;

let data_0 = transform test_0;;
let data_1 = transform test_1;;
let data = transform real;;


let is_not_here i lst =
  let rec nest = function
  | [] -> true
  | x::li -> if x == i then false else nest li
  in nest lst
;;

let solve_search k real =
  (*Search all combination*)
  let explore data lpast =
    let lst_trial = List.fold_left (fun mem i ->
      if is_not_here i lpast then i::mem else mem
      ) [] (List.init 5 (fun i -> i)) in

    List.map (fun i ->
      let data_i = Array.copy data in
      let opcode = silent_machine_state data_i i in
      (opcode, data_i, i::lpast)
      ) lst_trial
    in

  let rec nest ki lst =
  if ki == k then lst
  else (
    let choices = List.fold_left (fun mem (data_i, li) ->
      (explore data_i li) @ mem) [] lst in
    let maxi = List.fold_left (fun mem (op,_,_) -> max mem op) 0 choices in
    let max_choices = List.fold_left (fun mem (op, dat, conf) ->
      if op == maxi then (dat, conf)::mem else mem
    ) [] choices in
    nest (ki+1) max_choices
    )
  in
  let result = nest 0 [(transform real, [])] in
  (*let (_, r1) = List.hd result in*)
  List.iter (fun (_, r1) ->
    let data_x = transform real in
    List.iter (fun x ->
      let sign = silent_machine_state data_x x in
      Printf.printf "%d" sign
      ) (List.rev r1)
    ) result
  ;;
  (*
  List.sort (-) (List.map (fun (_, oplist) -> transform_oplist oplist) result)
  let (_, oplist)::lx = nest 0 [(transform real, [])] in
  List.iter (Printf.printf "%d") (List.rev oplist)
  *)
;;

let transform_oplist lst =
  List.fold_left (fun mem x -> mem * 10 + x) 0 (List.rev lst)
;;

Array.init 5 (fun i ->
  let data_x = transform real in
  let o0 = silent_machine_state data_x 1 (* 1 or 2*) in
  let o1 = silent_machine_state data_x 2 (* 1 or 2*) in
  let o2 = silent_machine_state data_x 3 (* 1 or 2*) in
  let o3 = silent_machine_state data_x 4 (* 1 or 2*) in

  Printf.printf "%d%d%d%d\n" o0 o1 o2 o3;
  (silent_machine_state data_x i, i)
  );;

(*66420 wrong and too low*)


let machine_state_phase data_ini phase inp =
  let config = ref (-1) in
  let data = Array.copy data_ini in
  (*Array.of_list (phase::inp::(Array.to_list data)) in*)

  let process_code op a b c i =
    if op == 1
    then (data.(c) <- a + b; i + 4)
    else if op == 2
    then (data.(c) <- a * b; i + 4)
    else if op == 3
    then (data.(a) <- phase; i + 2)
    else if op == 4
    then (
      Printf.printf "DIAG: %d\n" a;
      config := a mod 10;
      if a == 0
      then i+2
      else i+2 (*100000*)
      (*i + 2*)
      )
    else if op == 5
    then (if a != 0 then b else i + 3 )
    else if op == 6
    then (if a == 0 then b else i+3)
    else if op == 7
    then (data.(c) <- if a < b then  1 else 0; i+4 )
    else if op == 8
    then (data.(c) <- if a == b then  1 else 0; i+4)
    else if op == 99
    then (Printf.printf "Halting wih code 99\n"; i + 10000)
    else (Printf.printf "Error ! with code %d\n" op; i + 10000)
  in

  let l = Array.length data in

  let rec nest i =
    if i >= l then !config
    else (
      Printf.printf "%d:" i;
      let (op, a, b, c) = decompose data i in
      Printf.printf "\t%d (%d %d %d)\n" op a b c;
      let ip = process_code op a b c i in
      nest ip
      )
    in nest 0
;;
