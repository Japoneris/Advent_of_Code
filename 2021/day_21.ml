(*
Player 1 starting position: 6
Player 2 starting position: 7
*)

let position_to_score x =
  if x == 0 then 10 else x
;;

let roll p1 p2 =

  let rec nest round = function
  | (i1, s1)::pp::[] -> (
    let i2 = (i1 + round*3 + 3) mod 10 in
    let s2 = s1 + position_to_score i2 in
    if s2 >= 1000
    then (round+2, (i2,s2)::pp::[])
    else nest (round+3) (pp::(i2, s2)::[])
    ) in nest 1 ((p1, 0)::(p2, 0)::[])
;;
