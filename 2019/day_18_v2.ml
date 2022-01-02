
let read_input_data path =
  let ic = open_in path in
  let mat = Array.init 81 (fun _ ->
    let line = input_line ic in
    Array.init 81 (fun i ->
      line.[i]
      )
    ) in
  close_in_noerr ic;
  mat
;;

let print_puzzle mat =
  Array.iter (fun row ->
    Array.iter (Printf.printf "%c") row;
    print_newline ()
  ) mat
;;



type keydoo = Door of char | Key of char | Start | Block | Way | Unknown;;
type position   = {x:int; y:int};;
type objet = {p: position; o: keydoo; d:int};;

let process c =
  let ic = int_of_char c in
  if 'c' == '@'
  then Start
  else if c == '#'
  then Block
  else if (ic <= 90 ) && (ic >= 65)
  then Door (char_of_int (ic+32))
  else if (ic <=122) && (ic >= 97)
  then Key (char_of_int ic)
  else Way
;;



let get_next_position pt =
  List.map (fun (a,b) -> {x=pt.x+a; y=pt.y+b}) [(0, 1), (0, -1), (1, 0), (-1, 0)]
;;

let get_next_position_d pt =
  List.map (fun (a,b) -> {x=pt.x+a; y=pt.y+b; d=pt.d+1}) [(0, 1); (0, -1); (1, 0); (-1, 0)]
;;

let get_next_position pt =
  List.map (fun (a,b) -> {p={x=pt.p.x+a; y=pt.p.y+b}; d=pt.d+1; o=Unknown}) [(0, 1); (0, -1); (1, 0); (-1, 0)]
;;


let move_puzzle puzzle p0 =
  let mat = Array.map (fun row -> Array.copy row) puzzle in
  let fx = (get_next_position) in

  let rec nest mem = function
  | [] -> mem
  | pt::lst -> match process mat.(pt.p.y).(pt.p.x) with
    | Door c -> (
      mat.(pt.p.y).(pt.p.x) <- '#';
      nest ({o=Door c; p=pt.p; d=pt.d}::mem) (lst@(fx pt))
      )
    | Key c -> (
      mat.(pt.p.y).(pt.p.x) <- '#';
      nest ({o=Key c; p=pt.p; d=pt.d}::mem) (lst@(fx pt))
      )
    | Way | Start -> (
      mat.(pt.p.y).(pt.p.x) <- '#';
      nest mem (lst@(fx pt))
      )
    | Block -> nest mem lst
  in nest [] [{p=p0; o=Start; d=0}]
;;

(*Avoid passing through doors*)
let move_puzzle_limit puzzle p0 =
  let mat = Array.map (fun row -> Array.copy row) puzzle in
  let fx = (get_next_position) in

  let rec nest mem = function
  | [] -> mem
  | pt::lst -> match process mat.(pt.p.y).(pt.p.x) with
    | Door c -> (
      mat.(pt.p.y).(pt.p.x) <- '#';
      nest ({o=Door c; p=pt.p; d=pt.d}::mem) lst
      )
    | Key c -> (
      mat.(pt.p.y).(pt.p.x) <- '#';
      nest ({o=Key c; p=pt.p; d=pt.d}::mem) (lst @ (fx pt))
      )
    | Way | Start -> (
      mat.(pt.p.y).(pt.p.x) <- '#';
      nest mem (lst@(fx pt))
      )
    | Block -> nest mem lst
  in nest [] [{p=p0; o=Start; d=0}]
;;

(*Avoid passing through keys*)
let move_puzzle_limit_2 puzzle p0 =
  let mat = Array.map (fun row -> Array.copy row) puzzle in
  let fx = (get_next_position) in

  let rec nest mem = function
  | [] -> mem
  | pt::lst -> match process mat.(pt.p.y).(pt.p.x) with
    | Door c -> (
      mat.(pt.p.y).(pt.p.x) <- '#';
      nest ({o=Door c; p=pt.p; d=pt.d}::mem) (lst@(fx pt))
      )
    | Key c -> (
      mat.(pt.p.y).(pt.p.x) <- '#';
      nest ({o=Key c; p=pt.p; d=pt.d}::mem) lst
      )
    | Way | Start -> (
      mat.(pt.p.y).(pt.p.x) <- '#';
      nest mem (lst@(fx pt))
      )
    | Block -> nest mem lst
  in nest [] [{p=p0; o=Start; d=0}]
;;


(*4044 too high*)





(*Distance from key to all the neigborhood*)
(*
let results = List.map (fun pt ->
  match pt.o with
  | Key c  -> (Key c, move_puzzle_limit puzzle pt.p)
  | Door c -> (Door c, move_puzzle_limit_2 puzzle pt.p)
  | _ -> (Unknown, [])
  ) all_items
;;
*)


let print_start puzzle p0 =
  Printf.printf "START:\t";
  List.iter (fun p ->
    match p.o with
    | Key  c1->  Printf.printf "Key %c %d_" c1 p.d
    | Door c1 -> Printf.printf "Door %c %d_" c1 p.d
    ) (move_puzzle_limit puzzle p0)
;;

let get_area lst =
  let results = List.map (fun pt ->
    match pt.o with
    | Key c  -> (Key c, move_puzzle_limit puzzle pt.p)
    | Door c -> (Door c, move_puzzle_limit_2 puzzle pt.p)
    | _ -> (Unknown, [])
    ) lst in

  List.iter (fun (key, lst) ->
    let _ = (match key with
    | Key  c -> Printf.printf "Key %c:\t" c
    | Door c -> Printf.printf "Door %c:\t" c) in


    List.iter (fun p ->
      match p.o with
      | Key  c1->  Printf.printf "Key %c %d_" c1 p.d
      | Door c1 -> Printf.printf "Door %c %d_" c1 p.d
      ) (List.tl (List.rev lst));

      print_newline ()
    ) results
  ;;


let puzzle = read_input_data "input_18";;
(*Distance from start to anything*)
let all_items = move_puzzle puzzle {x=40; y=40};;

print_start puzzle {x=40; y=40};;
get_area all_items;;

(*4014 too high*)

(*See end in python*)

(*part 2*)

(*STOP at any item at any time*)
let move_puzzle_limit_3 puzzle p0 =
  let mat = Array.map (fun row -> Array.copy row) puzzle in
  let fx = (get_next_position) in

  mat.(p0.y).(p0.x) <- '.';

  let rec nest mem = function
  | [] -> mem
  | pt::lst -> match process mat.(pt.p.y).(pt.p.x) with
    | Door c -> (
      mat.(pt.p.y).(pt.p.x) <- '#';
      nest ({o=Door c; p=pt.p; d=pt.d}::mem) lst
      )
    | Key c -> (
      mat.(pt.p.y).(pt.p.x) <- '#';
      nest ({o=Key c; p=pt.p; d=pt.d}::mem) lst
      )
    | Way | Start -> (
      mat.(pt.p.y).(pt.p.x) <- '#';
      nest mem (lst@(fx pt))
      )
    | Block -> nest mem lst
  in nest [] [{p=p0; o=Start; d=0}]
;;

let lst = move_puzzle puzzle {x=39; y=39};;


let get_area_2 px = 
  (*Get all available items*)
  let lst = move_puzzle puzzle px in

  let results = List.map (fun pt ->
    (pt.o,  move_puzzle_limit_3 puzzle pt.p)
    ) ({p = px; o = Start; d = 0}::lst) in

  List.iter (fun (key, lst) ->
    let _ = (match key with
    | Key  c -> Printf.printf "Key %c:\t" c
    | Door c -> Printf.printf "Door %c:\t" c
    | _ -> Printf.printf "START:\t") in

    List.iter (fun p ->
      match p.o with
      | Key  c1->  Printf.printf "Key %c %d_" c1 p.d
      | Door c1 -> Printf.printf "Door %c %d_" c1 p.d
      ) (List.rev lst);

      print_newline ()
    ) results
  ;;



puzzle.(40).(40) <- '#';;
puzzle.(40).(41) <- '#';;
puzzle.(40).(39) <- '#';;
puzzle.(39).(40) <- '#';;
puzzle.(41).(40) <- '#';;

let part_tl = move_puzzle puzzle {x=39; y=39};;
let part_tr = move_puzzle puzzle {x=41; y=39};;
let part_bl = move_puzzle puzzle {x=39; y=41};;
let part_br = move_puzzle puzzle {x=41; y=41};;

(*
let all_items = move_puzzle puzzle {x=40; y=40};;
*)
(*Distance from key to all the neigborhood*)

print_start puzzle {x=39; y=39};;
get_area part_tl;;
print_start puzzle {x=41; y=39};;
get_area part_tr;;
print_start puzzle {x=39; y=41};;
get_area part_bl;;
print_start puzzle {x=41; y=41};;
get_area part_br;;
