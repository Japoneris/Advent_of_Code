



type target = {xm: int; xM: int; ym: int; yM: int};;

let targ_true = {xm=111; xM=161; ym=(-154); yM=(-101)};;

let update_y vy =
  vy - 1
;;

let update_x vx =
  if vx > 0 then vx - 1
  else if vx == 0 then vx
  else vx + 1
;;


let move vx0 vy0 t =
  let rec nest ymem xi yi vx vy =
  (*
    Printf.printf "(%d, %d)\t vx=%d\tvy=%d\n" xi yi vx vy;
    *)
    if yi < t.ym
    then (ymem, false)
    else if (yi <= t.yM) && (yi >= t.ym) && (xi >= t.xm) && (xi <= t.xM)
    then (ymem, true)
    else nest (max ymem yi) (xi + vx) (yi + vy) (update_x vx) (update_y vy)
  in nest 0 0 0 vx0 vy0
;;

let results = Array.init 10000 (fun i ->  (i, move 15 i targ_true));;

Array.fold_left (fun (i, mem) (j, (v, tf)) ->
  if tf then (
    if v >= mem then (j, v) else (i, mem)
    )
  else (i, mem) ) (0,0) results
;;


let check_one_x_velocity vx0 t =
  let rec nest xi vx =
  if (xi <= t.xM) && (xi >= t.xm)
  then true
  else if vx == 0 then false
  else nest (xi+vx) (update_x vx)
  in nest 0 vx0
;;

let check_one_y_velocity vy0 t =
  let rec nest yi vy =
  if (yi <= t.yM) && (yi >= t.ym)
  then true
  else if yi < t.ym then false
  else nest (yi+vy) (update_y vy)
  in nest 0 vy0
;;


let check_x_range t =
  (*NB: do not take into account negative location*)
  let lst = List.init (t.xM+1) (fun i -> (i+1, check_one_x_velocity (i+1) t)) in
  List.fold_left (fun mem (i, tf) -> if tf then i::mem else mem) [] lst
;;

let check_y_range t =
  (*times 2 + 3 to be sure to cover everything*)
  let lst = List.init ((abs t.ym) * 2 + 3) (fun i ->
    let vy = i + t.ym in (vy, check_one_y_velocity vy t)
    ) in
  List.fold_left (fun mem (i, tf) -> if tf then i::mem else mem) [] lst
;;



let check_all_combination xrg yrg t =

  let rec nest cnt = function
  | ([], []) -> cnt
  | ([], ly) -> cnt
  | (vx::lx, []) -> nest cnt (lx, yrg)
  | (vx::lx, vy::ly) ->
    let (_, tf) = move vx vy t in
    if tf
    then nest (cnt+1) (vx::lx, ly)
    else nest cnt (vx::lx, ly)
  in
  nest 0 (xrg, yrg)
;;

let xrange = check_x_range targ_true;;
let yrange = check_y_range targ_true;;
check_all_combination xrange yrange targ_true;;
