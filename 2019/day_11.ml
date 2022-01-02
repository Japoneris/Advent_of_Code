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



let solve data_in inp l =
  let data = Array.make l 0 in
  Array.iteri (fun i x -> data.(i) <- x) data_in;

  let process_code op a b c i base color =
  if op == 1
  then (data.(c) <- a + b; (i + 4, base, color))
  else if op == 2
  then ( data.(c) <- a * b; (i + 4, base, color))
  else if op == 3
  then (data.(a) <- inp; (i + 2, base, color))
  else if op == 4
  then (Printf.printf "TEST: %d\n" a; (i + 2, base, a::color))
  else if op == 5
  then (let res = if a != 0 then b else i + 3 in (res, base, color))
  else if op == 6
  then (let res = if a == 0 then b else i+3 in (res, base, color))
  else if op == 7
  then (data.(c) <- if a < b then  1 else 0; (i+4, base, color))
  else if op == 8
  then (data.(c) <- if a == b then  1 else 0; (i+4, base, color))
  else if op == 9
  then ((i + 2, base + a, color))
  else if op == 99
  then (Printf.printf "Halting wih code 99\n"; (i + l*2, base, color))
  else (Printf.printf "Error ! with code %d\n" op; (i + l*2, base, color))
  in

  let rec nest i base color_0 =
  (*
    Printf.printf "%d %d %d\n" i base inp;
    *)
    if i >= l then List.rev color_0
    else (
      (*
      Printf.printf "%d: \t(B %d) \t(dec: %d %d %d %d)\n" i base data.(i) data.(i+1) data.(i+2) data.(i+3);
      Printf.printf "\t%d (%d %d %d)\n" op a b c;
      *)
      let (op, a, b, c) = decompose data i base in
      let (ip, base_n, color_1) = process_code op a b c i base color_0 in
      nest ip base_n color_1
      )
    in nest 0 0 []
;;



type position = {x:int; y:int};;
type floor = {p:position; c:int};;
type direction =
| Top of position
| Down of position
| Left of position
| Right of position
;;

let move dir = function
| Top p -> if dir == 1
          then Right {x=p.x+1; y=p.y}
          else Left {x=p.x-1; y=p.y}
| Down p -> if dir == 1
          then Left {x=p.x-1; y=p.y}
          else Right {x=p.x+1; y=p.y}
| Left p -> if dir == 1
          then Top {x=p.x; y=p.y+1}
          else Down {x=p.x; y=p.y-1}
| Right p -> if dir == 1
          then Down {x=p.x; y=p.y-1}
          else Top {x=p.x; y=p.y+1}
;;

let process_moves lst =
  let rec nest mem p = function
  | c::d::lx -> nest (p::mem) (move d p) lx
  | [] -> mem
  in nest [] (Top {x=0; y=0}) lst
;;

let get_position = function
| Top p | Down p | Left p | Right p -> p
;;

let get_unique_position lst =
  let l1 = List.map (get_position) lst in
  let l2 = List.sort (fun a b ->
    if a.x > b.x then 1
    else if a.x == b.x then a.y - b.y
    else (-1)
    ) l1 in
  let (_, cmax) = List.fold_left (fun (mem, c) p ->
    if (mem.x == p.x) && (mem.y == p.y)
    then (mem, c)
    else (p, c+1)
  ) ({x=10000; y=10000}, 0) l2
  in cmax
;;

let data = read_input_data "input_11";;
let results = solve data (1000*Array.length data) 1;;
Printf.printf "Instructions: %d\n" (List.length results);;
let positions = process_moves results;;
let count = get_unique_position positions;;
(*498 with opcode 1, and 249 unique*)
(*21042 with opcode 2 but 11 unique*)

(*Mem is a floor list *)
let check_color x1 mem =
  let rec nest = function
  | [] -> 0
  | xi::lx ->
    if (xi.p.x == x1.x) && (xi.p.y == x1.y)
    then xi.c
    else nest lx
  in nest mem
;;


(*
let check_color x1 mem =
  List.fold_left (fun c xi ->
    if (xi.p.x == x1.x) && (xi.p.y == x1.y)
    then xi.c
    else c) 0 mem
;;
*)

let paint_floor p a mem =
  let rec nest old = function
  | [] -> {p=p; c=a}::mem
  | x::lx -> if (x.p.x == p.x) && (x.p.y == p.y)
    then {p=p; c=a}::(List.rev old)@lx
    else nest (x::old) lx
  in nest [] mem
;;


let solve data_in l c0 =
  let data = Array.make l 0 in
  Array.iteri (fun i x -> data.(i) <- x) data_in;

  let process_code op a b c i base inp =
  if op == 1
  then (data.(c) <- a + b; (i + 4, base))
  else if op == 2
  then ( data.(c) <- a * b; (i + 4, base))
  else if op == 3
  then (data.(a) <- inp; (i + 2, base))
  else if op == 5
  then (let res = if a != 0 then b else i + 3 in (res, base))
  else if op == 6
  then (let res = if a == 0 then b else i+3 in (res, base))
  else if op == 7
  then (data.(c) <- if a < b then  1 else 0; (i+4, base))
  else if op == 8
  then (data.(c) <- if a == b then  1 else 0; (i+4, base))
  else if op == 9
  then ((i + 2, base + a))
  else if op == 99
  then (Printf.printf "Halting wih code 99\n"; (i + l*2, base))
  else (Printf.printf "Error ! with code %d\n" op; (i + l*2, base))
  in

  let rec nest i base p f mem inp =
  (*
    Printf.printf "%d %d %d\n" i base inp;
    *)
    if i >= l then mem
    else (
      (*
      Printf.printf "%d: \t(B %d) \t(dec: %d %d %d %d)\n" i base data.(i) data.(i+1) data.(i+2) data.(i+3);
      Printf.printf "\t%d (%d %d %d)\n" op a b c;
      *)
      let (op, a, b, c) = decompose data i base in
      if op  == 4
      then (
        Printf.printf "TEST: %d\n" a;
        if f == 0
        then (*Move*) (
          let p1 = move a p in
          let x1 = get_position p1 in
          let input_1 = check_color x1 mem in
          nest (i+2) base p1 (1-f) (mem) input_1
          )
        else (*Paint the floor*) (
          (*a: new color / p: do not change*)
          let mem1 = paint_floor (get_position p) a mem in
          nest (i+2) base p (1-f) mem1 a
          )
        )
      else
      let (ip, base_n) = process_code op a b c i base inp in
      nest ip base_n  p f mem inp
      )
    in nest 0 0 (Top {x=0; y=0}) 1 [{p={x=0; y=0}; c=c0}] c0
    (*nest i base p f mem inp*)
;;



(*2008 not good, too low*)
let results = solve data (1000*Array.length data) 0;;
Printf.printf "Part 1: %d\n" (List.length results);;

let results = solve data (1000*Array.length data) 1;;



let print_mat mat =
Array.iter (fun x ->
  Array.iter (fun y -> if y == 0
      then Printf.printf "."
      else Printf.printf "|") x;
  print_newline ()
  ) mat ;
  print_newline ()
;;

let make_matrix min_x max_x min_y max_y =
  Array.init (max_y - min_y +1) (fun _ ->
  Array.make (max_x - min_x + 1) 0)
;;

let print_result results =
  let (min_x, max_x, min_y, max_y) = List.fold_left (fun (mx, mmx, my, mmy) p ->
    (min p.p.x mx, max p.p.x mmx, min p.p.y my, max p.p.y mmy)
  ) (100000, -100000, 100000, -100000) results in

  let mat = make_matrix min_x max_x min_y max_y in

  List.iter (fun p ->
    mat.(p.p.y - min_y).(p.p.x - min_x) <- p.c
    ) results;

    print_mat (Array.rev mat)
;;


print_result results;;

(*

...||.|..|..||..|||..|||...||...||..|..|...
....|.|..|.|..|.|..|.|..|.|..|.|..|.|..|...
....|.||||.|..|.|..|.|||..|....|....|..|...
....|.|..|.||||.|||..|..|.|.||.|....|..|...
.|..|.|..|.|..|.|.|..|..|.|..|.|..|.|..|...
..||..|..|.|..|.|..|.|||...|||..||...||....
*)

JHARBGCU
