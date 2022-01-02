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
(*
  else if data.(i) == 9
  then (9, data.(i+1), 0, 0) (*No base, default is one*)
  *)
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



let solve_2 data_in l message =
  let l0 = String.length message in
  let pi = ref 0 in

  let data = Array.make l 0 in
  Array.iteri (fun i x -> data.(i) <- x) data_in;

  let process_code op a b c i base =
  if op == 1
  then (
    data.(c) <- a + b;
    (i + 4, base))
  else if op == 2
  then (
    data.(c) <- a * b;
    (i + 4, base))
  else if op == 3
  then (
    let inp = if !pi == l0
    then (
      Printf.printf "Wanting input";
      print_newline (); read_int ())
    else (
      let cc = message.[!pi] in
      pi := !pi + 1;  int_of_char cc  ) in
    data.(a) <- inp;
    (i + 2, base))
  else if op == 4
  then (
    if a >= 128
    then (
      Printf.printf "__%d__" (a)
      )
    else (
      Printf.printf "%c" (char_of_int a)
      );

    (i + 2, base))
  else if op == 5
  then (
    let res = if a != 0 then b else i + 3 in
    (res, base))
  else if op == 6
  then (
    let res = if a == 0 then b else i+3
    in (res, base))
  else if op == 7
  then (data.(c) <- if a < b then  1 else 0;
    (i+4, base))
  else if op == 8
  then (data.(c) <- if a == b then  1 else 0;
    (i+4, base)
    )
  else if op == 9
  then (
    (i + 2, base + a)
    )
  else if op == 99
  then (
    Printf.printf "Halting wih code 99\n";
    (i + l*2, base)
    )
  else (
    Printf.printf "Error ! with code %d\n" op;
    (i + l*2, base)
    )
  in

  let rec nest i base =
    if i >= l then ()
    else (
      (*
      Printf.printf "%d: \t(B %d) \t(dec: %d %d %d %d)\n" i base data.(i) data.(i+1) data.(i+2) data.(i+3);
      Printf.printf "\t%d (%d %d %d)\n" op a b c;
      *)
      let (op, a, b, c) = decompose data i base in
      let (ip, base_n) = process_code op a b c i base  in
      nest ip base_n
      )
    in nest 0 0
;;


let mess = "NOT A J
NOT B T
AND T J
NOT C T
AND T J
AND D J
WALK
" ;;

let mess="NOT D J
WALK
"
;;

(*true if there is bricks, false if hoole *)
(*AND OR NOT *)
(*
- Si il y a un trou a 2 case NOT B xx
- Si il y a de la terre a 1 case OR A xx
*)

let good_p1="NOT C J
OR D T
AND T J
NOT A T
OR T J
WALK
"
;;

solve_2 data 10000 good_p1 ;;

(*
.................
......@..........
#####.#.##..#####
*)


(*
.................
.................
@................
#####.#.##..#####

.................
.................
.@...............
#####.#.##..#####

.................
.................
..@..............
#####.#.##..#####

.................
...@.............
.................
#####.#.##..#####

....@............
.................
.................
#####.#.##..#####

.................
.....@...........
.................
#####.#.##..#####

.................
.................
......@..........
#####.#.##..#####

.................
.......@.........
.................
#####.#.##..#####

........@........
.................
.................
#####.#.##..#####

.................
.........@.......
.................
#####.#.##..#####

.................
.................
..........@......
#####.#.##..#####

.................
.................
.................
#####.#.##@.#####

*)
let mess="NOT C J
OR D T
AND T J
NOT A T
OR T J
RUN
"
;;


let mess="NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
RUN
"
;;


let mess="NOT C J
NOT B T
OR T J
NOT A T
AND T J
RUN
"
;;

(*
@................
#####.###########
*)

let mess="NOT A J
RUN
"
;;

(*
........@........
#####.#..########
*)

let mess="OR D J
OR H T
AND T J
RUN
"
;;
(*
....@............
#####..###.#.####
     ABCDEFGHI
*)

let mess="OR D J
OR H T
OR I T
AND T J
AND A T
AND B T
AND C T
NOT T T
AND T J
RUN
"
;;

(*
....@............
#####..####...###
*)

let mess="NOT E T
AND H T
NOT F J
AND E J
AND I J
OR T J
AND D J
RUN
"
;;

(*
....@............
#####.###########
     ABCDEFGHI
*)


let mess="NOT E T
AND H T
OR E J
AND I J
OR T J
AND D J
RUN
"
;;
(*

..@..............
#####.##..#...###
   ABCDEFGHI
#####@##..#...###
     ABCDEFGHI

*)


let mess="OR C T
AND B T
AND A T
NOT T T
AND D T
OR H J
AND T J
RUN
"
;;
(*
....@............
#####...##...####
*)


let mess="OR C T
AND B T
AND A T
NOT T T
AND D T
OR H J
OR E J
AND T J
RUN
"
;;

1142627861

(*
..@..............
#####.#.##..#####

......@..........
#####.#.##..#####

..........@......
#####.#.##..#####
*)


(*
First jump: (will skip 3 steps, and arrive on the fourth)
x
#.#.##
 ABCDE
Second:
##..##
or
#..##


#####.#..########

*)

let mess="OR D T
OR B T
OR C T
OR E T
OR F T
OR G T
NOT T J
NOT A T
OR T J
RUN
"
;;



let solve_part_2 data_in l message =
  let l0 = String.length message in
  let pi = ref 0 in

  let data = Array.make l 0 in
  Array.iteri (fun i x -> data.(i) <- x) data_in;

  let process_code op a b c i base =
  if op == 1
  then (
    data.(c) <- a + b;
    (i + 4, base))
  else if op == 2
  then (
    data.(c) <- a * b;
    (i + 4, base))
  else if op == 3
  then (
    let inp = if !pi == l0
    then (
      Printf.printf "Wanting input";
      print_newline (); read_int ())
    else (
      let cc = message.[!pi] in
      pi := !pi + 1;  int_of_char cc  ) in
    data.(a) <- inp;
    (i + 2, base))
  else if op == 4
  then (
    if a >= 128
    then (
      Printf.printf "__%d__" (a)
      )
    else (
      Printf.printf "%c" (char_of_int a)
      );

    (i + 2, base))
  else if op == 5
  then (
    let res = if a != 0 then b else i + 3 in
    (res, base))
  else if op == 6
  then (
    let res = if a == 0 then b else i+3
    in (res, base))
  else if op == 7
  then (data.(c) <- if a < b then  1 else 0;
    (i+4, base))
  else if op == 8
  then (data.(c) <- if a == b then  1 else 0;
    (i+4, base)
    )
  else if op == 9
  then (
    (i + 2, base + a)
    )
  else if op == 99
  then (
    Printf.printf "Halting wih code 99\n";
    (i + l*2, base)
    )
  else (
    Printf.printf "Error ! with code %d\n" op;
    (i + l*2, base)
    )
  in

  let rec nest i base =
    if i >= l then ()
    else (
      (*
      Printf.printf "%d: \t(B %d) \t(dec: %d %d %d %d)\n" i base data.(i) data.(i+1) data.(i+2) data.(i+3);
      Printf.printf "\t%d (%d %d %d)\n" op a b c;
      *)
      let (op, a, b, c) = decompose data i base in
      let (ip, base_n) = process_code op a b c i base  in
      nest ip base_n
      )
    in nest 0 0
;;




let print_input mem =
  List.iter (fun x ->
    if x == 46
    then Printf.printf "."
    else if x == 35
    then Printf.printf "#"
    else if x == 10
    then print_newline ()
    else Printf.printf "%d" x

    ) (List.rev (List.tl mem))
;;

let list_to_mat lst =
  let (ltot, lcol) = List.fold_left (fun (m1, m0) x ->
    if x == 46
    then (m1, 0::m0)
    else if x == 35
    then (m1, 1::m0)
    else (m0::m1, [])
    ) ([], []) (List.tl (List.tl (lst)))
  in Array.of_list (List.map(Array.of_list) ltot)
;;

let get_intersect mat =
  let ly = Array.length mat
  and lx = Array.length mat.(0)
  in

  let mat_cnt = Array.mapi (fun y row ->
    if (y == 0) || (y == ly-1) then Array.make lx 0
    else (
      Array.mapi (fun x v ->
        if x == 0 then 0
        else if x == lx-1 then 0
        else if (v == 1) && (mat.(y).(x+1) == 1) && (mat.(y).(x-1) == 1) && (mat.(y+1).(x) == 1) && (mat.(y-1).(x) == 1)
        then 1
        else 0
        ) row
      )
    ) mat in
  Array.fold_left (fun mem r -> mem + (Array.fold_left (+) 0 r)) 0 mat_cnt
;;

let data = read_input_data "input_21";;
let mem = solve_2 data 0 10000;;

print_input mem;;
let mat = list_to_mat mem;;
get_intersect mat ;;


let data_test = transform "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99";;
let data_test_2 = transform "1102,34915192,34915192,7,4,7,99,0";;
let data_test_3 = transform  "104,1125899906842624,99";;

let data_05 = read_input_data "input_05";;
