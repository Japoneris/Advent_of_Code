let parse_line line =
  let l = String.length line in
  Array.init l (fun i -> int_of_char (line.[i]) - 48)
  ;;

let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    nest ((parse_line line)::lst)
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    Array.of_list (List.rev lst)
  in nest []
  ;;



let search_best_path mat =
  let l0 = Array.length mat
  and l1 = Array.length mat.(0) in

  let best = Array.init l0 (fun _ -> Array.make l1 10000000000) in

  let rec nest i j vx =
    if (i >= l0) || (i < 0) || (j >= l1) || (j < 0)
    then ()
    else if vx + mat.(i).(j) < best.(i).(j)
    then (
      let vy = vx + mat.(i).(j) in
      best.(i).(j) <- vy;
      nest (i+1) j vy;
      nest (i-1) j vy;
      nest i (j+1) vy;
      nest i (j-1) vy;
      ) in nest 0 0 0;
    best.(l0-1).(l1-1) - best.(0).(0)
;;


let mat = read_file "input_15_test";;
let mat = read_file "input_15.txt";;

let mx = search_best_path mat;;
Printf.printf "Result part 1: %d\n" mx.


let gen_big_map mat =
  let l0 = Array.length mat
  and l1 = Array.length mat.(0) in
  Array.init (l0*5) (fun i ->
    let i0 = i / l0
    and i1 = i mod l0 in

    Array.init (l1 * 5) (fun j ->
      let j0 = j / l1
      and j1 = j mod l1 in
        ((mat.(i1).(j1) - 1 + i0 + j0) mod 9) + 1
      )
    )
;;

let print_mat mat =
  Array.iter (fun row ->
    Array.iter (fun x -> Printf.printf "%d" x) row;
    print_newline ()
    ) mat
;;





type pos_score = {x:int; y:int; v:int};;

let insert p lst =
  let rec nest mem = function
  | [] -> lst@[p]
  | pi::lx ->
    if pi.v > p.v then (List.rev mem) @ [p; pi] @ lx
    else nest (pi::mem) lx
  in nest [] lst
;;

let gen_nearby p =
  let x = p.x
  and y = p.y
  and v = p.v in
  {x=x+1; y=y; v=v}::{x=x-1; y=y; v=v}::{x=x; y=y+1; v=v}::{x=x; y=y-1; v=v}::[]
;;
  

let search_best_path_efficient mat =
  let l0 = Array.length mat
  and l1 = Array.length mat.(0) in

  let best = Array.init l0 (fun _ -> Array.make l1 10000000000) in

  let rec nest = function
  | [] -> ()
  | p::lst ->
    if (p.x >= l0) || (p.x < 0) || (p.y >= l1) || (p.y < 0)
    then nest lst
    else if (p.x == (l0-1)) && (p.y == l1-1)
    then ( (*First solution, so the best normally*)
      best.(p.x).(p.y) <- p.v + mat.(p.x).(p.y)
      ) (*Ealy finish*)
    else (
      let v1 = p.v + mat.(p.x).(p.y) in
      if v1 < best.(p.x).(p.y)
      then (
        best.(p.x).(p.y) <- v1; (*Save the information*)
        let l1 = gen_nearby {x=p.x; y=p.y; v=v1} in
        let l2 = List.fold_left (fun mem a -> insert a mem) lst l1 in
        nest l2
        )
      else nest lst (*Do nothing as not good enough*)
      )
  in
  nest [{x=0; y=0; v=0}];
  best.(l0-1).(l1-1) - best.(0).(0)
;;



let mat = read_file "input_15.txt";;
let mat5 = gen_big_map mat;;
let mx = search_best_path_efficient mat5;;
