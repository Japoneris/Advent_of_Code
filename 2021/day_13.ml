let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    nest (( line)::lst)
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    (List.rev lst)
  in nest []
  ;;

type position = {x: int; y:int};;
type fold =
  | X of int
  | Y of int
;;

let parse_position x =
  let a::b::[] = String.split_on_char ',' x in
  {x= int_of_string a; y= int_of_string b}
;;

let parse_fold x =
  let (a::b::c::[]) = String.split_on_char ' ' x in
  let (ci::cj::[]) = String.split_on_char '=' c in
  if String.equal ci "y"
  then Y (int_of_string cj)
  else X (int_of_string cj)
;;

let parse_file lst =
  let rec nest mem0 mem1 = function
  | [] -> (mem0, List.rev mem1)
  | x::lx ->
    if String.length x < 2
    then nest mem0 mem1 lx
    else if String.contains x ','
    then nest ((parse_position x)::mem0) mem1 lx
    else nest mem0 ((parse_fold x)::mem1) lx
  in nest [] [] lst
;;



let build_map instr =
  let x1 = 1 + (List.fold_left (fun mem x -> max mem x.x) 0 instr)
  and y1 = 1 + (List.fold_left (fun mem x -> max mem x.y) 0 instr) in

  let x2 = if x1 mod 2 == 0 then x1+1 else x1
  and y2 = if y1 mod 2 == 0 then y1+1 else y1 in

  let mat = Array.init x2 (fun _ -> Array.make y2 false) in

  List.iter (fun p -> mat.(p.x).(p.y) <- true) instr;
  mat
;;

let fold_map mat fold =
  let l0 = Array.length mat
  and l1 = Array.length mat.(0) in

  match fold with
  | X x -> (

    Array.init x (fun i ->
      Array.init l1 (fun j ->
        mat.(i).(j) || mat.(l0-x+i-1).(j)
        )
      )
    )
  | Y x -> (
    Array.init l0 (fun i ->
      Array.init x (fun j ->
        mat.(i).(j) || mat.(i).(l1-j-1)
        )
      )
    )
;;


let show_mat grid =
  Array.iteri (fun j row ->
    Array.iteri (fun i x ->
      if grid.(i).(j) then Printf.printf "#" else Printf.printf ".") grid;
    print_newline()
    ) grid.(0)
;;

let count_solution grid =
  Array.fold_left (fun mem row ->
    Array.fold_left (fun m1 x -> if x then m1+1 else m1) mem row
    ) 0 grid
;;

let lst = read_file "input_13_test";;
let lst = read_file "input_13.txt";;
let (inst0, inst1) = parse_file lst;;

let mat = build_map inst0;;
let maty = fold_map mat (List.hd inst1);;
let res = count_solution maty;;
Printf.printf "Solution part 1: %d\n" res;;

let matx = List.fold_left (fun mem x -> fold_map mem x) mat inst1;;
show_mat matx;;

(*CEJKLUGJ*)
