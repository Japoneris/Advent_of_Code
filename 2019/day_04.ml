let vmin = 109165
and vmax = 576723;;

(*
Rule 1:
increase <= left to right

Rule 2:
At least 2 repeating digits
*)


let decompose x =
  let rec nest lst v =
  if v == 0 then lst
  else nest ((v mod 10)::lst) (v/10)
  in nest [] x
;;


let is_double_crement x =
  let rec nest cc = function
    | [] -> cc
    | i::[] ->  cc
    | a::b::lst -> if a == b
      then nest true (b::lst)
      else if a > b
      then false
      else nest cc (b::lst)
  in nest false x
;;


let is_single_double lst =
  let rec check_remove c v = function
  | [] -> if c == 2
    then (true, true, [])
    else (false, true, [])
  | x::lx -> if x == v
    then check_remove (c+1) v lx
    else (c==2 , x > v, x::lx)
  in

  let rec nest cc = function
  | [] -> cc
  | i::[] ->  cc
  | a::lx ->
    let (t1, t2, ly) = check_remove 1 a lx in
    if t2
    then nest (t1 || cc) ly
    else false
in nest false lst
;;


let solve_1 vmin vmax =
  let rec nest c v =
  if v == vmax then c
  else (
    let dec = decompose v in
    let ci = if is_double_crement dec then 1 else 0 in
    nest (c+ci) (v+1)
    ) in nest 0 vmin
;;


let solve_2 vmin vmax =
  let rec nest c v =
  if v == vmax then c
  else (
    let dec = decompose v in
    let ci = if is_single_double dec then 1 else 0
    in
    nest (c+ci) (v+1)
    ) in nest 0 vmin
;;
