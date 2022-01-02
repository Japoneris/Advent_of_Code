
type vvv =
  | I of int
  | C of char
;;

type operation =
  | Input of vvv
  | Add of vvv * vvv
  | Mul of vvv * vvv
  | Div of vvv * vvv
  | Mod of vvv * vvv
  | Eql of vvv * vvv
;;

let get_v stri =
  if stri.[0] == 'w' then C 'w'
  else if stri.[0] == 'x' then C 'x'
  else if stri.[0] == 'y' then C 'y'
  else if stri.[0] == 'z' then C 'z'
  else I (int_of_string stri)
;;

let parse_line line =
  if line.[0] == 'i'
  then (
    let (word::letter::[]) = String.split_on_char ' ' line in
    Input (get_v letter)
    )
  else if line.[0] == 'a'
  then (
    let (word::la::lb::[]) = String.split_on_char ' ' line in
    Add (get_v la, get_v lb)
    )
  else if line.[0] == 'd'
  then (
    let (word::la::lb::[]) = String.split_on_char ' ' line in
    Div (get_v la, get_v lb)
    )
  else if line.[0] == 'e'
  then (
    let (word::la::lb::[]) = String.split_on_char ' ' line in
    Eql (get_v la, get_v lb)
    )
  else if line.[1] == 'o'
  then (
    let (word::la::lb::[]) = String.split_on_char ' ' line in
    Mod (get_v la, get_v lb)
    )(*mod*)
  else (*mul*) (
    let (word::la::lb::[]) = String.split_on_char ' ' line in
    Mul (get_v la, get_v lb)
    )
  ;;

let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    nest (parse_line line ::lst)
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    (List.rev lst)
  in nest []
;;

let lst = read_file "input_24.txt";;

let update_records v field w x y z =
  match field with
  | C 'w' -> (v, x, y, z)
  | C 'x' -> (w, v, y, z)
  | C 'y' -> (w, x, v, z)
  | C 'z' -> (w, x, y, v)
  | _ -> (
    Printf.printf "Error in update %d\n" v;
    (w, x, y, z)
    )
;;

let get_val op w x y z =
  match op with
  | C 'w' -> w
  | C 'x' -> x
  | C 'y' -> y
  | C 'z' -> z
  | I i -> i
;;


let run_program inpt_arr lst =

  let rec nest i w x y z = function
  | [] -> (w, x, y, z)
  | Add (op0, op1)::linstr -> (
    let v0 = get_val op0 w x y z
    and v1 = get_val op1 w x y z in
    let (w1, x1, y1, z1) = update_records (v0+v1) op0 w x y z in
    nest i w1 x1 y1 z1 linstr
    )
  | Input op0 ::linstr -> (
    Printf.printf "Previous state (%d): %d %d %d %d\n" i w x y z;
    let (w1, x1, y1, z1) = update_records inpt_arr.(i) op0 w x y z in
    nest (i+1) w1 x1 y1 z1 linstr
    )
  | Div (op0, op1)::linstr -> (
    let v0 = get_val op0 w x y z
    and v1 = get_val op1 w x y z in
    let (w1, x1, y1, z1) = update_records (v0 / v1) op0 w x y z in
    nest i w1 x1 y1 z1 linstr
    )
  | Eql (op0, op1)::linstr -> (
    let v0 = get_val op0 w x y z
    and v1 = get_val op1 w x y z in
    Printf.printf "%d == %d ?\n" v0 v1;
    let (w1, x1, y1, z1) = update_records (if v0 == v1 then 1 else 0) op0 w x y z in
    nest i w1 x1 y1 z1 linstr
    )
  | Mul (op0, op1)::linstr -> (
    let v0 = get_val op0 w x y z
    and v1 = get_val op1 w x y z in
    let (w1, x1, y1, z1) = update_records (v0 * v1) op0 w x y z in
    nest i w1 x1 y1 z1 linstr
    )
  | Mod (op0, op1)::linstr -> (
    let v0 = get_val op0 w x y z
    and v1 = get_val op1 w x y z in
    Printf.printf "%d mod %d ?\n" v0 v1;
    let (w1, x1, y1, z1) = update_records (v0 mod v1) op0 w x y z in
    nest i w1 x1 y1 z1 linstr
    ) in nest 0 0 0 0 0 lst
  ;;


let run_program_silent inpt_arr lst =

  let rec nest i w x y z = function
  | [] -> (w, x, y, z)
  | Add (op0, op1)::linstr -> (
    let v0 = get_val op0 w x y z
    and v1 = get_val op1 w x y z in
    let (w1, x1, y1, z1) = update_records (v0+v1) op0 w x y z in
    nest i w1 x1 y1 z1 linstr
    )
  | Input op0 ::linstr -> (
    let (w1, x1, y1, z1) = update_records inpt_arr.(i) op0 w x y z in
    nest (i+1) w1 x1 y1 z1 linstr
    )
  | Div (op0, op1)::linstr -> (
    let v0 = get_val op0 w x y z
    and v1 = get_val op1 w x y z in
    let (w1, x1, y1, z1) = update_records (v0 / v1) op0 w x y z in
    nest i w1 x1 y1 z1 linstr
    )
  | Eql (op0, op1)::linstr -> (
    let v0 = get_val op0 w x y z
    and v1 = get_val op1 w x y z in
    let (w1, x1, y1, z1) = update_records (if v0 == v1 then 1 else 0) op0 w x y z in
    nest i w1 x1 y1 z1 linstr
    )
  | Mul (op0, op1)::linstr -> (
    let v0 = get_val op0 w x y z
    and v1 = get_val op1 w x y z in
    let (w1, x1, y1, z1) = update_records (v0 * v1) op0 w x y z in
    nest i w1 x1 y1 z1 linstr
    )
  | Mod (op0, op1)::linstr -> (
    let v0 = get_val op0 w x y z
    and v1 = get_val op1 w x y z in
    let (w1, x1, y1, z1) = update_records (v0 mod v1) op0 w x y z in
    nest i w1 x1 y1 z1 linstr
    ) in nest 0 0 0 0 0 lst
  ;;





let seed =   [|9; 9; 9; 9; 1; 1;1;1; 1; 1; 1; 8; 5;4|];;
run_program seed lst;;

let update_arr arr =
  let rec nest i =
  if arr.(i) == 1
  then (arr.(i) <- 9; nest (i-1))
  else (
    arr.(i) <- arr.(i)-1;
    arr
  ) in nest 13
;;

let print_arr arr =
  Array.iter (Printf.printf "%d") arr;
  print_newline ()
;;

let search_best puzzle =
  let seed = Array.make 14 9 in

  let rec nest  arr =
    let (w, x, y, z) = run_program_silent seed lst in
    if z == 0 then arr
    else (
      print_arr arr;
      nest (update_arr arr)
      )
  in nest seed
;;

(*Study outcome blocks by blocks*)
let split_blocks lst =
  let rec nest mem buff = function
  | [] -> List.tl (List.rev ((List.rev buff)::mem)) (*.tl because first block empty*)
  | Input op::linstr -> nest ((List.rev buff)::mem) ((Input op)::[]) linstr
  | xx::linstr -> nest mem (xx::buff) linstr
  in nest [] [] lst
;;



let run_program_block i0 lst z0 =

  let rec nest w x y z = function
  | [] -> (w, x, y, z)
  | Add (op0, op1)::linstr -> (
    let v0 = get_val op0 w x y z
    and v1 = get_val op1 w x y z in
    let (w1, x1, y1, z1) = update_records (v0+v1) op0 w x y z in
    nest w1 x1 y1 z1 linstr
    )
  | Input op0 ::linstr -> (
    let (w1, x1, y1, z1) = update_records i0 op0 w x y z in
    nest  w1 x1 y1 z1 linstr
    )
  | Div (op0, op1)::linstr -> (
    let v0 = get_val op0 w x y z
    and v1 = get_val op1 w x y z in
    let (w1, x1, y1, z1) = update_records (v0 / v1) op0 w x y z in
    nest w1 x1 y1 z1 linstr
    )
  | Eql (op0, op1)::linstr -> (
    let v0 = get_val op0 w x y z
    and v1 = get_val op1 w x y z in
    let (w1, x1, y1, z1) = update_records (if v0 == v1 then 1 else 0) op0 w x y z in
    nest w1 x1 y1 z1 linstr
    )
  | Mul (op0, op1)::linstr -> (
    let v0 = get_val op0 w x y z
    and v1 = get_val op1 w x y z in
    let (w1, x1, y1, z1) = update_records (v0 * v1) op0 w x y z in
    nest w1 x1 y1 z1 linstr
    )
  | Mod (op0, op1)::linstr -> (
    let v0 = get_val op0 w x y z
    and v1 = get_val op1 w x y z in
    let (w1, x1, y1, z1) = update_records (v0 mod v1) op0 w x y z in
    nest w1 x1 y1 z1 linstr
    ) in nest 0 0 0 z0 lst
  ;;


let blocks = split_blocks lst;;

let bl0::bl1::bl2::bl3::bl4::blother =  blocks;;


List.init 9 (fun i -> (i+1, run_program_block (i+1) bl0 0));;




let clean_list lst =
  let l1 = List.sort (-) lst in
  let rec nest mem = function
  | [] -> mem
  | x::[] -> x::mem
  | x::y::ll -> if x == y
    then nest mem (y::ll)
    else nest (x::mem) (y::ll)
  in nest [] l1
;;

let check_single_solution blocks =
  let rec nest mem = function
  | [] -> mem
  | blk::lll -> (
    let l1 = List.fold_left (fun m1 z0 ->
      let ll = List.init 9 (fun i ->
        let (_, _, _, z1) = run_program_block [|i+1|] blk z0 in z1
        ) in
        ll @ m1
      ) [] mem in
    let l2 = clean_list l1 in
    Printf.printf "Round: %d => %d" (List.length l1) (List.length l2);
    print_newline ();
    nest l2 lll
    ) in nest [0] blocks
;;


let blocks = split_blocks lst;;

let bl0::bl1::bl2::bl3::bl4::blother =  blocks;;

let l0 = List.init 9 (fun i ->
  let (_, _, _, z) = run_program_silent [|i+1|] bl0 in
  z);;

let l1 = List.fold_left (fun mem z0 ->
  let ll = List.init 9 (fun i ->
    let (_, _, _, z1) = run_program_block [|i+1|] bl1 z0 in z1
    ) in
    ll @ mem
  ) [] l0;;

let l2 = List.fold_left (fun mem z0 ->
  let ll = List.init 9 (fun i ->
    let (_, _, _, z1) = run_program_block [|i+1|] bl2 z0 in z1
    ) in
    ll @ mem
  ) [] l1;;


let l3 = List.fold_left (fun mem z0 ->
  let ll = List.init 9 (fun i ->
    let (_, _, _, z1) = run_program_block [|i+1|] bl3 z0 in z1
    ) in
    ll @ mem
  ) [] l2;;



(*

92961693948791 too low

99999876432195 too high

99999999999995


*)
