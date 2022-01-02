let card_key = 16915772;;
let door_key = 18447943;;

20201227

let get_loop subject v_target  =
  let rec nest i v =
  if v == v_target then i
  else (
    let vi = (v*subject) mod 20201227 in
    nest (i+1) vi
    )
  in nest 0 1
;;

let make_loop subject n_loop =
  let rec nest i v =
  if i == n_loop then v
  else (
    let vi = (v*subject) mod 20201227 in
    nest (i+1) vi
    )
  in nest 0 1
  ;;

let loop_1 = get_loop 7 card_key;;
let loop_2 = get_loop 7 door_key;;
