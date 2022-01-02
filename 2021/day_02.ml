type move =
  | Up of int
  | Down  of int
  | Forward of int
;;

let to_move line =
  let (a::b::[]) = String.split_on_char ' ' line in
  let bi = int_of_string b in
  if a.[0] == 'f' then Forward bi
  else if a.[0] == 'd' then Down bi
  else Up bi
;;


(*
    forward X increases the horizontal position by X units.
    down X increases the depth by X units.
    up X decreases the depth by X units.
*)



let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    nest ((to_move line)::lst)
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    (List.rev lst)
  in nest []
  ;;

let get_depth lst =
  let rec nest h d = function
  | [] -> (h, d)
  | (Up x)::lst -> nest h (d-x) lst
  | (Down x)::lst -> nest h (d+x) lst
  | (Forward x)::lst -> nest (h+x) d lst
  in nest 0 0 lst
;;


let lst = read_file "input_02.txt";;
let (h, d) = get_depth lst;;
Printf.printf "Horizontal: %d\n" h;;
Printf.printf "Depth: %d\n" d;;
Printf.printf "Product: %d\n" (d * h);;


(*
Part 2

-    down X increases your aim by X units.
-    up X decreases your aim by X units.
-    forward X does two things:
    -    It increases your horizontal position by X units.
    -    It increases your depth by your aim multiplied by X.
*)

let get_depth_aim lst =
  let rec nest h d a = function
  | [] -> (h, d)
  | (Up x)::lst -> nest h d (a-x) lst
  | (Down x)::lst -> nest h d (a+x) lst
  | (Forward x)::lst -> nest (h+x) (d + a * x) a lst
  in nest 0 0 0 lst
;;

let (h, d) = get_depth_aim lst;;
Printf.printf "Horizontal: %d\n" h;;
Printf.printf "Depth: %d\n" d;;
Printf.printf "Product: %d\n" (d * h);;
