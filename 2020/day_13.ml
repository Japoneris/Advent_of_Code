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

let l1::l2::[] = read_file "input_13";;

let ids = List.fold_left (fun mem x ->
  if String.equal x "x"
  then mem
  else (int_of_string x)::mem
  ) [] (String.split_on_char  ',' l2);;

let t0 = int_of_string l1;;


let solve_1 t0 lst =
  let l3 = List.map (fun x ->
    let t1 = t0 mod x in
    (t0 - t1 + x, x)
    ) lst in
 let (t1, v1)::_ = List.sort (fun (a1, v1) (a2, v2) -> a1-a2) l3 in
 (t1  - t0) * v1

;;

let t_test = 939;;
let test = [7;13;59;31;19];;

solve_1 t0 ids;;


let (_, ids_2) = List.fold_left (fun (idx, mem) x ->
  if String.equal x "x"
  then (idx+1, mem)
  else (idx+1, (idx, int_of_string x)::mem)
  ) (0, []) (String.split_on_char  ',' l2);;


let check t1 lst =
  let rec nest = function
  | [] -> true
  | (i, x)::lx ->
    if (t1+i) mod x == 0 then nest lx
    else false
  in nest lst
;;

let test_2 = [(0, 7);(1, 13);(4, 59); (6,31);(7, 19)];;


let solve_2 t0 lst_2 =
  let (imax, vmax)::_ = List.sort (fun (a, va) (b, vb) -> (vb-va)) lst_2 in


  let (i2, v2)::lst = List.sort (fun (a, va) (b, vb) -> (a-b)) lst_2 in

  let rec nest k =
  let t1 = v2*k in
    Printf.printf "%d: %d" k t1;
    print_newline ();
    if check t1 lst_2
    then t1
    else nest (k+v2)
  in nest 738
;;

let check_coeffs x0 x1 off1 =
  let rec nest i =
  let v = x1*i + off1 in
  if v mod x0 == 0
  then i
  else nest (i+1)
  in nest 0
;;

let convert_equation x0 x1 off1 =
  let v1 = check_coeffs x0 x1 off1 in
  Printf.printf "alpha=%d * x + %d\n" x1 ((v1*x1+ off1) / x0);
  (v1*x1+ off1) / x0
;;

let solve_2 lst_2 =
  let (i2, v2)::lst = List.sort (fun (a, va) (b, vb) -> (a-b)) lst_2 in

  let l2 = List.map (fun (i, x) ->
  let a = convert_equation v2 x i in
  (x, a)
  ) lst in
  List.fold_left (fun (m1, p1) (m, p) ->
    (m1 * m, m*p1 + m1*p)
  ) (v2, 0) lst
;;

let update_equation x0 v0 x1 v1 =
  let f = x0 * x1 in
  let c2 = check_coeffs x0 x1 (v1 - v0) in
  (f, c2*x0)
;;



let check_coeffs x0 x1 off1 =
  let rec nest i =
  let v = x1*i + off1 in
  if v mod x0 == 0
  then i
  else nest (i+1)
  in nest 0
;;


let check_coeffs_2 a0 b0 a1 b1 =
  let delta = b1 - b0 in

  let rec nest i =
  if (delta + i*a1) mod a0 == 0
  then i
  else nest (i+1)
  in nest 0
;;


let check_coeffs_3 a0 b0 a1 b1 =
  let delta = b1 - b0 in
  if a0 > a1
  then (
    let rec nest i =
    if (-delta + i*a0) mod a1 == 0
    then (-delta + i*a0) / a1
    else nest (i+1)
    in nest 0

    )
  else (
    let rec nest i =
    if (delta + i*a1) mod a0 == 0
    then i
    else nest (i+1)
    in nest 0
    )
;;

let solve_2 lst_2 =
  let (i2, v2)::lst = List.sort (fun (a, va) (b, vb) -> (a-b)) lst_2 in

  List.fold_left (fun (off1, m1) (off, m) ->
    Printf.printf "%d %d: %d %d" m1 off1 m off;
    print_newline ();
    let c = check_coeffs_3 m1 (off1) m (-off) in
    ((-off + m*c), m * m1)
  ) (0, v2) lst
;;
