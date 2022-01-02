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


(*


The remote control program executes the following steps in a loop forever:

    Accept a movement command via an input instruction.
    Send the movement command to the repair droid.
    Wait for the repair droid to finish the movement operation.
    Report on the status of the repair droid via an output instruction.

*)

(*


Only four movement commands are understood:
north (1),
south (2),
west (3),
and east (4).
Any other command is invalid.

The movements differ in direction,
but not in distance: in a long enough east-west hallway,
a series of commands like 4,4,4,4,3,3,3,3
would leave the repair droid back where it started.
*)

(*
The repair droid can reply with any of the following status codes:

    0: The repair droid hit a wall. Its position has not changed.
    1: The repair droid has moved one step in the requested direction.
    2: The repair droid has moved one step in the requested direction; its new position is the location of the oxygen system.
*)

type position = {x:int; y:int};;

let check_dir robot c =
if c == 1 then (true, {x=robot.x; y=robot.y + 1})
else if c == 2 then (true, {x=robot.x; y=robot.y - 1})
else if c == 3 then (true, {x=robot.x-1; y=robot.y})
else if c == 4 then (true, {x=robot.x+1; y=robot.y})
else (Printf.printf "ERROR direction %d\n" c; (false, robot))
;;

let use_mem robot = function
| s::x::[] ->
  let (tf, robot_1) = check_dir robot  x in
  if tf then (
    if s == 0
    then (
      Printf.printf "Could no move at \t%d %d" robot_1.x robot_1.y;
      (false, robot, [])
    )
    else if s == 1
    then (false, robot_1, [])
    else (true, robot_1, [])
  ) else (false, robot, [])
| lx -> (false, robot, lx)
;;

let check_car x =
  if x == 3
  then '#'
  else if x == 0
  then '.'
  else if x == 1
  then 'o'
  else if x == 2
  then 'X'
  else '?' (*-1*)
;;


let print_matrix mat =
  Array.iter (fun row ->
    Array.iter (fun x ->
      Printf.printf "%c" (check_car x)
      ) row;
    print_newline ()
    ) mat
;;

let move_on_mat robot mat = function
  | s::x::[] -> (
    Printf.printf "%d %d\n" s x;

    let (tf, r1) = check_dir robot x in
    if tf then (
      let dx = (Array.length mat)/2 in
      if s == 0
      then (
        mat.(r1.y + dx).(r1.x + dx) <- 3;
        print_matrix mat;
        (false, robot, [])
        )
      else if s == 1
      then (
        (*
        mat.(r1.y + dx).(r1.x + dx) <- 1;
        mat.(robot.y + dx).(robot.x + dx) <- 0;
        print_matrix mat;
        *)
        if mat.(r1.y+dx).(r1.x + dx) == 0
        then (
          mat.(r1.y + dx).(r1.x + dx) <- 1;
          mat.(robot.y + dx).(robot.x + dx) <- (-1);
          print_matrix mat
          )
        else (
          mat.(r1.y + dx).(r1.x + dx) <- 1;
          mat.(robot.y + dx).(robot.x + dx) <- 0;
          print_matrix mat
          );
        (false, r1, [])
      ) else (
        mat.(r1.y + dx).(r1.x + dx) <- 2;
        print_matrix mat;
        (true, r1, [])
        )
      )
    else (false, robot, [])
    )
  | lx -> (false, robot, lx)
;;


let select_direction mat robot d =
  let ddd = (Array.length mat)/2 in

  let x = robot.x +ddd
  and y = robot.y +ddd in

  let bc = 3 in

  if d == 2 (*Go up*)
  then (
    if (mat.(y).(x-1) != bc)
    then 3
    else if (mat.(y-1).(x) != bc)
    then 2
    else if (mat.(y).(x+1) != bc)
    then 4
    else 1 (*Back*)
    )
  else if d == 1 (*Go down*)
  then (
    if (mat.(y).(x+1) != bc)
    then 4
    else if (mat.(y+1).(x) != bc)
    then 1
    else if (mat.(y).(x-1) != bc)
    then 3
    else 2 (*Opposite dir*)
    )
  else if d == 3
  then (
    if (mat.(y+1).(x) != bc)
    then 1
    else if (mat.(y).(x-1) != bc)
    then 3
    else if (mat.(y-1).(x) != bc)
    then 2
    else 4
    )
  else ( (*d=4*)
    if (mat.(y-1).(x) != bc)
    then 2
    else if (mat.(y).(x+1) != bc)
    then 4
    else if (mat.(y+1).(x) != bc)
    then 1
    else 3
    )
;;


let solve data_in l =

  let data = Array.make l 0 in
  Array.iteri (fun i x -> data.(i) <- x) data_in;
  let mat = Array.init 50 (fun _ -> Array.make 50 (-1)) in


  let pc = ref 0 in (*past code *)
  let dd = ref 1 in (*past direction*)
  let rb = ref {x=0; y=0} in (*past robot position*)

  let process_code op a b c i base mem  =
  if op == 1
  then (data.(c) <- a + b; (i + 4, base, mem))
  else if op == 2
  then (data.(c) <- a * b; (i + 4, base, mem))
  else if op == 3
  then (

    (*
    let i_asked = (
      try
        max (min (read_int ()) 4) 1
      with e -> 1 ) in
    *)

    Printf.printf "Past dir: %d, past pos: %d %d\n" (!dd) (!rb).x (!rb).y;


    let i_asked = select_direction mat (!rb) (!dd) in
    dd := i_asked;

    Printf.printf "Iasked: %d\n" i_asked;
    print_newline ();
    data.(a) <- i_asked; (i + 2, base, i_asked::mem)
    )
  else if op == 4
  then (Printf.printf "TEST: %d\n" a;
    pc := a;
    (i + 2, base, a::mem)
  )
  else if op == 5
  then (let res = if a != 0 then b else i + 3 in (res, base, mem))
  else if op == 6
  then (let res = if a == 0 then b else i+3 in (res, base, mem))
  else if op == 7
  then (data.(c) <- if a < b then  1 else 0; (i+4, base, mem))
  else if op == 8
  then (data.(c) <- if a == b then  1 else 0; (i+4, base, mem))
  else if op == 9
  then ((i + 2, base + a, mem))
  else if op == 99
  then (
    Printf.printf "END 99\n";
    (i + l*2, base, mem))
  else (
    Printf.printf "Error ! Code %d\n" op;
    (i + l*2, base, mem))
  in


  let rec nest i base mem robot =
    if i >= l then mem
    else (
      (*
      Printf.printf "\t%d (%d %d %d)\n" op a b c;
      print_newline ();
      *)
      let (op, a, b, c) = decompose data i base in
      let (ip, base_n, mem_1) = process_code op a b c i base mem in
      let (tf, robot_1, mem_2) = move_on_mat robot mat mem_1 in

      rb := robot_1;
      if tf
      then (Printf.printf "Found reservoir at (%d %d)\n" robot_1.x robot_1.y; mem)
      else (nest ip base_n mem_2 robot_1)
      )
    in nest 0 0 [] {x=0; y=0}
;;


let data = read_input_data "input_15";;
let data_test = transform "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99";;
let data_test_2 = transform "1102,34915192,34915192,7,4,7,99,0";;
let data_test_3 = transform  "104,1125899906842624,99";;

let data_05 = read_input_data "input_05";;


let move_on_mat_p2 robot mat = function
  | s::x::[] -> (
    Printf.printf "%d %d\n" s x;

    let (tf, r1) = check_dir robot x in
    if tf then (
      let dx = (Array.length mat)/2 in
      if s == 0
      then (
        mat.(r1.y + dx).(r1.x + dx) <- 3;
        print_matrix mat;
        (false, robot, [])
        )
      else if s == 1
      then (

          mat.(r1.y + dx).(r1.x + dx) <- 1;
          mat.(robot.y + dx).(robot.x + dx) <- 0;
          print_matrix mat;

        (false, r1, [])
      ) else (
        mat.(r1.y + dx).(r1.x + dx) <- 3;
        print_matrix mat;
        (true, r1, [])
        )
      )
    else (false, robot, [])
    )
  | lx -> (false, robot, lx)
;;

let solve_2 data_in l =

  let data = Array.make l 0 in
  Array.iteri (fun i x -> data.(i) <- x) data_in;
  let mat = Array.init 50 (fun _ -> Array.make 50 (-1)) in


  let pc = ref 0 in (*past code *)
  let dd = ref 1 in (*past direction*)
  let rb = ref {x=0; y=0} in (*past robot position*)

  let process_code op a b c i base mem  =
  if op == 1
  then (data.(c) <- a + b; (i + 4, base, mem))
  else if op == 2
  then (data.(c) <- a * b; (i + 4, base, mem))
  else if op == 3
  then (

    Printf.printf "Past dir: %d, past pos: %d %d\n" (!dd) (!rb).x (!rb).y;


    let i_asked = select_direction mat (!rb) (!dd) in
    dd := i_asked;

    Printf.printf "Iasked: %d\n" i_asked;
    print_newline ();
    data.(a) <- i_asked; (i + 2, base, i_asked::mem)
    )
  else if op == 4
  then (Printf.printf "TEST: %d\n" a;
    pc := a;
    (i + 2, base, a::mem)
  )
  else if op == 5
  then (let res = if a != 0 then b else i + 3 in (res, base, mem))
  else if op == 6
  then (let res = if a == 0 then b else i+3 in (res, base, mem))
  else if op == 7
  then (data.(c) <- if a < b then  1 else 0; (i+4, base, mem))
  else if op == 8
  then (data.(c) <- if a == b then  1 else 0; (i+4, base, mem))
  else if op == 9
  then ((i + 2, base + a, mem))
  else if op == 99
  then (
    Printf.printf "END 99\n";
    (i + l*2, base, mem))
  else (
    Printf.printf "Error ! Code %d\n" op;
    (i + l*2, base, mem))
  in


  let rec nest i base mem robot =
    if i >= l then mem
    else (
      (*
      Printf.printf "\t%d (%d %d %d)\n" op a b c;
      print_newline ();
      *)
      let (op, a, b, c) = decompose data i base in
      let (ip, base_n, mem_1) = process_code op a b c i base mem in
      let (_, robot_1, mem_2) = move_on_mat_p2 robot mat mem_1 in

      rb := robot_1;
      nest ip base_n mem_2 robot_1
      )
    in nest 0 0 [] {x=0; y=0}
;;
