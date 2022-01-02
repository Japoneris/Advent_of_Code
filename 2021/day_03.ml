let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    nest ( line::lst)
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    (List.rev lst)
  in nest []
;;


let count_ones lst =
  let c0 = List.fold_left (fun mem x -> if x == '1' then mem + 1 else mem) 0 lst in
  if c0 > (List.length lst)/2
  then 1
  else 0
;;


let count_ones_b lst =
  let c0 = List.fold_left (fun mem x -> if x == '1' then mem + 1 else mem) 0 lst in
  let l1 = List.length lst in
  if c0 >= l1 - c0
  then 1
  else 0
;;


let count_ones_c lst =
  let c0 = List.fold_left (fun mem x -> if x == '1' then mem + 1 else mem) 0 lst in
  let l1 = List.length lst in
  if c0 >= l1 - c0
  then 0
  else 1
  ;;



let get_binary_code lst =
  List.init (String.length (List.hd lst)) (fun i  ->
    let l1 = List.map (fun y -> y.[i]) lst in
    count_ones l1
  )
;;

let reconstruct_int lst_10 =
  List.fold_left (fun mem x -> mem * 2 + x) 0 lst_10
;;

let reconstruct_int_str lst_10s =
  let l0 = String.length lst_10s in
  let l1 = List.init l0 (fun i -> if lst_10s.[i] == '1' then 1 else 0) in
  reconstruct_int l1
;;


let inverse lst_10 =
  List.map (fun x -> (1-x)) lst_10
;;



let lst = read_file "input_03.txt";;

let bin_code = get_binary_code lst;;
let bin_code_inv = inverse bin_code;;

let gamma = reconstruct_int bin_code;;
let epsilon = reconstruct_int bin_code_inv;;

Printf.printf "Gamma: %d\n" gamma;;
Printf.printf "Epsilon: %d\n" epsilon;;
Printf.printf "G * E: %d\n" (gamma * epsilon);;



(*Part 2: Filter based on the maximal bit*)

let filter_max lst =
  let s = String.length (List.hd lst) in
  let rec nest i lx =
    if i == s then lx
    else (
      let l1 = List.map (fun x -> x.[i]) lx in
      let bi = count_ones_b l1 in (*b version ensure sup or equal => 1*)
      let ly = if bi == 1
        then List.fold_left (fun mem x -> if x.[i] == '1' then x::mem else mem) [] lx
        else List.fold_left (fun mem x -> if x.[i] == '0' then x::mem else mem) [] lx
        in
      nest (i+1) ly
      )
    in nest 0 lst
  ;;

let filter_min lst =
  let s = String.length (List.hd lst) in
  let rec nest i lx =
    if List.length lx == 1
    then lx
    else if i == s then lx
    else (
      let l1 = List.map (fun x -> x.[i]) lx in
      let bi = count_ones_c l1 in (*b version ensure sup or equal => 1*)
      let ly = if bi == 1
        then List.fold_left (fun mem x -> if x.[i] == '1' then x::mem else mem) [] lx
        else List.fold_left (fun mem x -> if x.[i] == '0' then x::mem else mem) [] lx
        in
      nest (i+1) ly
      )
    in nest 0 lst
  ;;



(*Normally, l1 is empty*)
let (bin_max::l1) = filter_max lst;;
let (bin_min::l1) = filter_min lst;;

let oxygen = reconstruct_int_str bin_max;;
let co2 = reconstruct_int_str bin_min;;

Printf.printf "Oxygen: %d\n" oxygen;;
Printf.printf "CO2: %d\n" co2;;
Printf.printf "0 * C: %d\n" (oxygen* co2);;

(*481800 Too low*)
