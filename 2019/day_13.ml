let scanint x = Scanf.sscanf x "%d " (fun y -> y);;
let transform x = Array.of_list (List.map (scanint) (String.split_on_char ',' x));;


let read_input_data path =
  let ic = open_in path in
  let stri  = input_line ic in
  close_in_noerr ic;
  transform stri
;;

let decompose data i base =
  if data.(i) == 99
  then (99, 0, 0, 0)
  else (
    let (op, inter) = (data.(i) mod 100, data.(i) / 100) in
    if (op <= 2) || ((op > 4) && (op < 9))
    then (
      let ga = inter mod 10
      and gb = (inter / 10) mod 10
      and gc = (inter / 100) mod 10
      in

      let a = if ga == 0
              then data.(data.(i+1))
              else if ga == 2 then data.(data.(i+1) + base)
              else data.(i+1)
      and b = if gb == 0
              then data.(data.(i+2))
              else if gb == 2 then data.(data.(i+2) + base)
              else data.(i+2)
      and c = if gc == 0  (*No 1 normaly*)
              then data.(i+3)
              else data.(i+3) + base in
      (op, a, b, c)
      )
      else if (op == 9)
      then (
        let res = if inter mod 10 == 0 then data.(data.(i+1))
                  else if inter mod 10 == 1 then data.(i+1)
                  else data.(data.(i+1) + base) in
        (9, res, 0, 0)
        )
      else if op == 4
      then (
        let res = if inter mod 10 == 0 then data.(data.(i+1))
                  else if inter mod 10 == 1 then data.(i+1)
                  else data.(data.(i+1) + base) in
        (4, res, 0, 0)
        )
      else if (op == 3)
      then (
        let res = if inter mod 10 == 0 then data.(i+1)
                  else if inter mod 10 == 1 then i+1
                  else data.(i+1) + base in
        (op, res, 0, 0))
      else (
        (op, data.(i+1), 0, 0)
        )
    )
;;


let solve data_in inp l =

  let data = Array.make l 0 in
  Array.iteri (fun i x -> data.(i) <- x) data_in;

  let process_code op a b c i base mem =
  if op == 1
  then (
    data.(c) <- a + b;
    (i + 4, base, mem))
  else if op == 2
  then (
    data.(c) <- a * b;
    (i + 4, base, mem))
  else if op == 3
  then (
    data.(a) <- inp;
    (i + 2, base, mem))
  else if op == 4
  then (
    Printf.printf "TEST: %d\n" a;
    (i + 2, base, a::mem))
  else if op == 5
  then (
    let res = if a != 0 then b else i + 3 in
    (res, base, mem))
  else if op == 6
  then (
    let res = if a == 0 then b else i+3
    in (res, base, mem))
  else if op == 7
  then (data.(c) <- if a < b then  1 else 0;
    (i+4, base, mem))
  else if op == 8
  then (data.(c) <- if a == b then  1 else 0;
    (i+4, base, mem)
    )
  else if op == 9
  then (
    (i + 2, base + a, mem)
    )
  else if op == 99
  then (
    Printf.printf "Halting wih code 99\n";
    (i + l*2, base, mem)
    )
  else (
    Printf.printf "Error ! with code %d\n" op;
    (i + l*2, base, mem)
    )
  in

  let rec nest i base mem =
    if i >= l then mem
    else (
      (*
      Printf.printf "%d: \t(B %d) \t(dec: %d %d %d %d)\n" i base data.(i) data.(i+1) data.(i+2) data.(i+3);
      Printf.printf "\t%d (%d %d %d)\n" op a b c;
      *)
      let (op, a, b, c) = decompose data i base in
      let (ip, base_n, mem_b) = process_code op a b c i base mem in
      nest ip base_n mem_b
      )
    in nest 0 0 []
;;

type action = {x:int; y:int; a:int};;

let transform_outputs lst =
  let rec nest mem = function
  | x::y::t::lx -> nest ({x=x;y=y;a=t}::mem) lx
  | _ -> mem
  in
  nest [] (List.rev lst)
;;

let count_unique k lst =

  let equal a b =
    if a.x== b.y then a.y - b.y else a.x - b.y
  in
  let l0 = List.fold_left (fun mem g -> if g.a == k then g::mem else mem) [] lst in
  let l1 = List.sort (equal) l0 in

  let rec nest c = function
  | x::y::lx -> nest (if (equal x y) == 0 then c else c+1) (y::lx)
  | y::[] -> c
  | [] -> c in nest 0 l1
;;

let count_lst lst x =
  List.fold_left (fun mem v -> if v.a == x then mem+1 else mem) 0 lst
;;


let data = read_input_data "input_13";;
let results = solve data 0 100000;;
let lst_actions = transform_outputs results;;
let cyy = List.init 5 (fun i -> (i, count_lst lst_actions i));;

(*0:*)
(*827 too high*)
(*340 too low*)

(*
let count row x =
  Array.fold_left (fun mem v -> if v == x then mem+1 else mem) 0 row
;;

let make_map lst_a =
  let (minx, maxx, miny, maxy) = List.fold_left (fun (minx, maxx, miny, maxy) g ->
    (min minx g.x, max maxx g.x, min miny g.y, max maxy g.y)
  ) (10000, 0, 10000, 0) lst_a in
  let mat = Array.init (maxy+1) (fun i -> Array.make (maxx+1) (-1)) in

  List.iter (fun g -> mat.(g.y).(g.x) <- g.a) lst_a;

  let c0 = Array.fold_left (fun mem row -> mem + (count row 0)) 0 mat in
  let c1 = Array.fold_left (fun mem row -> mem + (count row 1)) 0 mat in
  let c2 = Array.fold_left (fun mem row -> mem + (count row 2)) 0 mat in
  let c3 = Array.fold_left (fun mem row -> mem + (count row 3)) 0 mat in
  let c4 = Array.fold_left (fun mem row -> mem + (count row 4)) 0 mat in

  Printf.printf "Blocks visible (%d %d %d %d %d)\n" c0 c1 c2 c3 c4;

  (mat, (minx, maxx, miny, maxy))
;;

*)



List.iteri (fun i g -> if g.a > 4 then Printf.printf "%d: %d\n" i g.a) lst_actions


let solve_p2 data_in l =
  let data = Array.make l 0 in
  Array.iteri (fun i x -> data.(i) <- x) data_in;
  data.(0) <- 2;

  let inp = ref 0 in
  let joy = ref 0 in

  let process_mem = function
  | t::y::x::[] -> (
    (*Printf.printf "%d %d %d\n" x y t;*)
    if (x == (-1)) && (y == 0)
    then (Printf.printf "Score at %d: %d\n" t data.(t);)
    else if t == 3
    then (joy := x)
    else if t == 4
    then (
      inp := (if (!joy) > x
          then (-1)
          else if (!joy) < x
          then 1
          else 0
      ));

  [])
  | lst -> lst
  in

  let process_code op a b c i base mem =
  if op == 1
  then (data.(c) <- a + b; (i + 4, base, mem))
  else if op == 2
  then (data.(c) <- a * b; (i + 4, base, mem))
  else if op == 3
  then (data.(a) <- !inp; (i + 2, base, mem))
  else if op == 4
  then (
    (i + 2, base, process_mem (a::mem))
  )
  else if op == 5
  then (let res = if a != 0 then b else i + 3 in (res, base, mem))
  else if op == 6
  then (let res = if a == 0 then b else i+3 in (res, base, mem))
  else if op == 7
  then (data.(c) <- if a < b then  1 else 0;(i+4, base, mem))
  else if op == 8
  then (data.(c) <- if a == b then  1 else 0;(i+4, base, mem))
  else if op == 9
  then ((i + 2, base + a, mem))
  else if op == 99
  then (Printf.printf "Halting wih code 99\n";
    (i + l*2, base, mem))
  else ( Printf.printf "Error ! with code %d\n" op;
    (i + l*2, base, mem))
  in

  let rec nest i base mem =
    if i >= l then mem
    else (
      let (op, a, b, c) = decompose data i base in
      let (ip, base_n, mem_b) = process_code op a b c i base mem in
      nest ip base_n mem_b
      )
    in nest 0 0 []
;;
