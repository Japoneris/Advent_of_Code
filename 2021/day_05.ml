type position = {x: int; y: int};;
type wind = {start: position; out: position};;

let parse_line line =
  let (p1::_::p3::[]) = String.split_on_char ' ' line in
  let a1::a2::[] = String.split_on_char ',' p1
  and b1::b2::[] = String.split_on_char ',' p3 in
  {start={x=int_of_string a1; y= int_of_string a2}; out={x=int_of_string b1; y=int_of_string b2}}
  ;;


let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    nest ((parse_line line)::lst)
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    (List.rev lst)
  in nest []
  ;;



let filter_part_1 lines =
  List.fold_left (fun mem posi ->
    if (posi.start.x == posi.out.x) || (posi.start.y == posi.out.y)
    then posi::mem else mem
    ) [] lines
;;



let filter_part_2 lines =
  List.fold_left (fun mem posi ->
    if (posi.start.x == posi.out.x) || (posi.start.y == posi.out.y)
    then mem else posi::mem
    ) [] lines
;;

let get_dims lines =
  List.fold_left (fun (x1, x2, y1, y2) x ->
    let xx1 = min x.start.x x.out.x
    and xx2 = max x.start.x x.out.x
    and yy1 = min x.start.y x.out.y
    and yy2 = max x.start.y x.out.y in

    (min x1 xx1, max x2 xx2, min y1 yy1, max y2 yy2)

  ) (0, 0, 0, 0) lines
;;

(* (0, 990, 0, 984)*)

let update_row mat i m0 m1 =
  let _ = Array.init (m1 - m0 + 1) (fun j -> mat.(i).(m0+j) <- mat.(i).(m0+j) + 1) in
  ()
;;

let update_col mat i m0 m1 =
  let _ = Array.init (m1 - m0 + 1) (fun j -> mat.(m0+j).(i) <- mat.(m0+j).(i) + 1) in
  ()
;;



let count_above mat x =
  Array.fold_left (fun mem row ->
    Array.fold_left (fun m2 v -> if v >= x then m2+1 else m2) mem row
    ) 0 mat
  ;;

let fill_the_board lines s =
  let mat = Array.init s (fun _ -> Array.init s (fun _ -> 0)) in
  List.iter (fun posi ->
    if posi.start.x == posi.out.x
    then update_col mat posi.start.x (min posi.start.y posi.out.y) (max posi.start.y posi.out.y)
    else update_row mat posi.start.y (min posi.start.x posi.out.x) (max posi.start.x posi.out.x)
    ) lines;
  mat
;;



let update_diag mat pos =
  let sx = if pos.start.x > pos.out.x then -1 else 1
  and sy = if pos.start.y > pos.out.y then -1 else 1
  and dt = abs  (pos.start.x - pos.out.x) in
  (*
  Printf.printf "%d %d %d\n" sx sy dt;
  Printf.printf "%d %d :: %d %d\n" pos.start.x pos.out.x pos.start.y pos.out.y;
  *)
  let _ = Array.init (1+dt) (fun i ->
    mat.(pos.start.y + sy * i).(pos.start.x + sx * i) <-  mat.(pos.start.y + sy * i).(pos.start.x + sx * i) + 1;
    ) in
  ()
;;



let fill_the_board_diag lines mat =
  List.iter (fun x -> update_diag mat x) lines;
  mat
;;








let lst = read_file "input_05_test";;
let s = 10;;

let lst = read_file "input_05_test";;
let s = 10;;

let lst = read_file "input_05.txt";;
let s = 1000;;


let lst1 = filter_part_1 lst ;;
let matx = fill_the_board lst1 s;;
count_above matx 2;;


let lst2 = filter_part_2 lst ;;
let matx = fill_the_board_diag lst2 matx;;
count_above matx 2;;

(*TOO high 20886 / TOO low 18418*)
