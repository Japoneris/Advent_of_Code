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

let data = read_file "input_18";;


type equ =
| Parenthesis of equ list
| Val of int
| Mul
| Add
;;

(*
  (7 * (3 + 8 + 8 + 7) + (6 + 8 * 2 + 5 + 2 * 6) * (5 + 2) * 9) + ((7 * 4 + 8) * 6 * 8 + 9) * 7 * 2 * 2
  6 * ((9 + 4) * (6 * 7 + 5 + 8 * 2))
  *)

let read_equation line =
  let lmax = String.length line in

  let rec nest i lst =
  if i == lmax then (i, lst)
  else if line.[i] == ' '
  then nest (i+1) lst
  else if line.[i] == '('
  then (
    let (i1, lst1) = nest (i+1) [] in
    nest (i1+1) ((Parenthesis lst1)::lst)
    )
  else if line.[i] == ')'
  then (i, lst)
  else if line.[i] == '*'
  then nest (i+1) ((Mul)::lst)
  else if line.[i] == '+'
  then nest (i+1) ((Add)::lst)
  else ( (*int value*)
    let v = int_of_string (String.sub line i 1) in
    nest (i+1) ((Val v)::lst)
    )
  in nest 0 []
;;

let make_product lst =
  let rec nest v = function
  | [] -> v
  | x::lx -> nest (v*x) lx
  in nest 1 lst
;;

let make_product2 lst =
  let rec nest v = function
  | [] -> v
  | (Val x)::lx -> nest (v*x) lx
  in nest 1 lst
;;


let reduce_equation_2 equa =
  let rec nest mem = function
  | [] -> Val (make_product2 mem)
  | (Val x)::[] -> Val (make_product2 ((Val x)::mem))
  | (Parenthesis lx)::ly -> (
    let v1 = nest [] lx in
    nest mem (v1::ly)
    )
  | a::op::(Parenthesis lx)::ly -> (
    let v1 = nest [] lx in
    nest mem (a::op::v1::ly)
    )
  | (Val x)::Mul::(Val y)::ly -> nest ((Val x)::mem) ((Val y)::ly)
  | (Val x)::Add::(Val y)::ly -> nest mem ((Val (x+y))::ly)
in nest [] equa
;;

let transform_eq line =
  let (_, eq) = read_equation line in
  reduce_equation_2 eq
;;

List.fold_left (fun mem line ->
  let (Val v) = transform_eq line in
  Printf.printf "%d + %d\n" mem v;
  mem + v
  ) 0 data;;
