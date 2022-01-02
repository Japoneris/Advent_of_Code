let seed = [3;6;8;1;9;5;7;4;2];;

let solve_1 lst nmax =

  let rec nest i lx =
  if i == nmax
  then lx
  else match lx with
  | [] -> lx
  | x::ly -> (
    List.iter (Printf.printf "%d,") (x::ly);
    print_newline ();
    (*Select the group of cup*)
    (*P1: subset of preempted cups*)
    (*P2: leftover*)
    let (p1, p2) = select_cup 3 ly in
    let (p3, p4) = search_new_pos x p2 in

    nest (i+1) (p3@p1@p4@[x])
    )
  in nest 0 lst
;;

let select_cup n lst =
  let rec nest i mem = function
  | [] -> (List.rev mem, [])
  | x::lx ->
    if i == n
    then (List.rev mem, x::lx)
    else nest (i+1) (x::mem) lx
  in nest 0 [] lst
;;

let search_new_pos v lst =

  let rec nest v1 =
  if v1 == 0
  then nest 9
  else
  let (mm0, mm1) = List.fold_left (fun (m0, m1) x ->
    if x == v1 then (x::m1, [])
    else (m0, x::m1)
    ) ([], []) lst in
  if List.length mm0 > 0
  then (List.rev mm0, List.rev mm1)
  else nest (v1-1)
  in nest v
;;

let seed_t = [3;8;9;1;2;5;4;6;7];;


[95648732; 1]

let seed_2 = seed @ (Array.to_list
  ( Array.init (1000000 - 10) (fun i -> i+10)));;



let search_new_pos_v2 v lst vlim =

  let rec nest v1 =
  if v1 == 0
  then nest vlim
  else
  let (mm0, mm1) = List.fold_left (fun (m0, m1) x ->
    if x == v1 then (x::m1, [])
    else (m0, x::m1)
    ) ([], []) lst in
  if List.length mm0 > 0
  then (List.rev mm0, List.rev mm1)
  else nest (v1-1)
  in nest v
;;

let select_top_k k lst =
  let rec nest ki mem lx =
  if k == ki then List.rev mem
  else match  lx with
  | x::lx -> nest (ki+1) (x::mem) lx
  in nest 0 [] lst
;;

let solve_2 lst nmax =
  let l = List.length lst in

  let rec nest i lx =
  if i == nmax
  then lx
  else match lx with
  | [] -> lx
  | x::ly -> (
    List.iter (Printf.printf "%d,") (select_top_k 10 (x::ly));
    print_newline ();
    (*Select the group of cup*)
    (*P1: subset of preempted cups*)
    (*P2: leftover*)
    let (p1, p2) = select_cup 3 ly in
    let (p3, p4) = search_new_pos_v2 x p2 l in

    nest (i+1) (p3@p1@p4@[x])
    )
  in nest 0 lst
;;


let seed_at = [|3;8;9;1;2;5;4;6;7|];;
let seed_a = [|3;6;8;1;9;5;7;4;2|];;

let check_next_position v lnot l =
  (*l number of items*)
  let rec nest vi =
  if vi < 0 then nest (l-1)
  else
    if (List.fold_left (fun mem x ->
    if vi == x then true else mem) false lnot)
    then nest (vi - 1)
    else vi
  in nest v
;;

let solve_2 seed ktot rmax =
  let l = Array.length seed in
  (*Setup the follow neighbors*)
  let a_next = Array.init ktot (fun i ->
    if i >= l then i+1 else 0) in
  Array.iteri (fun i x ->
    if i == l - 1
    then a_next.(x-1) <- l
    else a_next.(x-1) <- seed.(i+1) -1
    ) seed;
  a_next.(ktot-1) <- seed.(0) - 1;

  let a_back = Array.make ktot 0 in
  Array.iteri (fun i x -> a_back.(x) <- i) a_next;


  let rec nest r v =
  if r == rmax then (v, a_next, a_back)
  else (
    (*
    print_array a_next v;
    Three items to move*)
    let v1 = a_next.(v) in
    let v2 = a_next.(v1) in
    let v3 = a_next.(v2) in
    let vnext = a_next.(v3) in (*The one to start next*)

    (*Check where to include the cards*)
    let v_incrust = check_next_position (v-1) [v1; v2; v3] ktot in
    (*
    Printf.printf "Current: %d\t Next: %d\n" (v+1) (vnext+1);
    Printf.printf "Take : [%d, %d, %d]\n" (v1+1) (v2+1) (1+v3);
    Printf.printf "Moved to : %d\n" (1+v_incrust);
    *)

    let v_tmp = a_next.(v_incrust) in
    a_next.(v_incrust) <- v1;
    a_back.(v1) <- v_incrust;
    a_back.(v_tmp) <- v3;
    a_next.(v3) <- v_tmp;

    a_next.(v) <- vnext;
    a_back.(vnext) <- v;
    nest (r+1) vnext
    )
  in nest 0 (seed.(0) - 1)
;;

let (a1, r1, r2) = solve_2 seed_a 1000000 10000000;;
let v1 = r1.(0)+1 in
let v2 = r1.(v1-1)+1 in
Printf.printf "%d x %d = %d\n" v1 v2 (v1 * v2);;


let print_array arr_next v0 =
  let l = Array.length arr_next in
  let rec nest v i =
  if (i >= l)  then ()
  else (
    Printf.printf "%d," (1+v);
    nest (arr_next.(v)) (i+1)
    )
  in nest v0 0;
  print_newline ()
;;
