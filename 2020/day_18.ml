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

let rec check_equation line =
(*
  Printf.printf "Checking %s\n" line;
  *)

  let lmax = String.length line in

  let rec nest i v op =
  if i == lmax then (i, v)
  else if line.[i] == '('
  then (
    (*i2: value from end parenthesis*)
    let (i2, v2) = check_equation
      (String.sub line (i+1) ((String.length line) -i-1))
    in
    nest (i+i2+1) (op v v2) op
    )
  else if line.[i] == ')'
  then (
    (*
    Printf.printf "Line %s\n" line;
    Printf.printf "\t%d\n" v;
    *)
    (i+1, v)) (*Return*)
  else if line.[i] == ' '
  then nest (i+1) v op
  else if line.[i] == '*'
  then nest (i+1) v (fun a b -> a*b)
  else if line.[i] == '+'
  then nest (i+1) v (fun a b -> a+b)
  else (
    let vx = int_of_string (String.sub line i 1) in
    nest (i+1) (op v vx) op
    )
  in nest 0 0 (fun a b -> a + b)
  ;;

List.fold_left (fun mem line ->
  let (_, v) = check_equation line in
  mem + v
  ) 0 data;;


type equation =
| Mul
| Add
| Val of int
;;

type equation =
| Mult of equation * equation
| Add of equation * equation
| Par of equation
| Val of int
;;


let reduce_equation lst =
  let rec nest v = function
  | [] -> Val v
  | (Val x)::lx -> nest x lx
  | Mul::Val(x)::lx -> nest (v*x) lx
  | Add::Val(x)::lx -> nest (v+x) lx

  in nest 0 lst
;;

let get_value = function
| Mul -> 0
| Add -> (-1)
| Val x -> x
;;


let rec check_plus line =
  let lmax = String.length line in

  let rec nest i lst =
  if i == lmax
  then (i, reduce_equation lst)
  else if line.[i] == ')'
  then  (i, reduce_equation lst)
  else if line.[i] == '*'
  then nest (i+1) (Mul::lst)
  else if line.[i] == '+'
  then nest (i+1) (Add::lst)
  else if line.[i] == '('
  then (
    let s1 = String.sub line (i+1) (String.length line - (i+1)) in
    let (i1, v1) = check_plus s1 in
    (*
    Printf.printf "Recover %d in %s\n" (get_value v1) (String.sub s1 0 i1);
    *)
    nest (i+i1+2) (v1::lst)
    (*Open parenthesis*)
    )
  else if line.[i] == ' '
  then nest (i+1) lst
  else (
    let vx = int_of_string (String.sub line i 1) in
    match lst with
    | [] ->
      nest (i+1) [Val vx]
    | Mul::(Val x)::lx ->
      nest (i+1) ((Val vx)::Mul::(Val x)::lx) (*No update*)
    | Add::(Val x)::lx ->
      nest (i+1) ((Val (x + vx))::lx)
    )
  in nest 0 []
;;


List.fold_left (fun mem line ->
  let (_, v) = check_plus line in
  let v2 = (get_value v) in
  Printf.printf "%d + %d\n" mem v2;
  mem + v2
  ) 0 data;;


List.fold_left (fun mem line ->
  let (_, v) = check_plus line in
  mem + (get_value v)
  ) 0 data;;

(*Too low 533557245186920*)
