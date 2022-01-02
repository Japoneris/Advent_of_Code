let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    nest (line::lst)
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    (List.rev lst)
  in nest []
  ;;

type direction =
| North of int
| South of int
| East of int
| West of int
| Left of int
| Right of int
| Forward of int
;;

let letter_to_dir x y =
(if x == 'N' then North y
else if x == 'S' then South y
else if x == 'E' then East y
else if x == 'W' then West y
else if x == 'L' then Left y
else if x == 'R' then Right y
else Forward y)
;;


let data = read_file "input_12";;
let numbers = List.map (fun line ->
     Scanf.sscanf line "%c%d" (fun x y -> letter_to_dir x y)
     ) data;;

let solve_1 instruction =
  (*Dir: number in 0 / 180*)
  (*Dir=0: east *)
  let rec nest dir r c = function
  | [] -> (r, c)
  | (North i)::lx -> nest dir (r+i) c lx
  | (South i)::lx -> nest dir (r-i) c lx
  | (East i)::lx -> nest dir r (c+i) lx
  | (West i)::lx -> nest dir r (c-i) lx
  | (Left i)::lx -> nest ((dir+i) mod 360) r c lx
  | (Right i)::lx -> nest ((dir-i+360) mod 360) r c lx
  | (Forward i)::lx ->
    if dir == 0 then nest dir r (c+i) lx
    else if dir == 90 then nest dir (r+i) c lx
    else if dir == 180 then nest dir r (c-i) lx
    else nest dir (r-i) c lx
  in nest 0 0 0 instruction
;;


let solve_2 instruction =
  (*Dir: number in 0 / 180*)
  (*Dir=0: east *)
  (*rw: position of waypoint*)
  let rec nest rw cw rs cs lx =
  Printf.printf "%d %d %d %d\n" rw cw rs cs;
  match lx with
  | [] -> (rs, cs)
  | (North i)::lx -> nest  (rw+i) cw rs cs lx
  | (South i)::lx -> nest  (rw-i) cw rs cs lx
  | (East i)::lx -> nest  rw (cw+i) rs cs lx
  | (West i)::lx -> nest  rw (cw-i) rs cs lx
  | (Right i)::lx ->
    if i == 0 then nest rw cw rs cs lx
    else if i == 90  then  nest (-cw) rw rs cs lx
    else if i == 180 then nest (-rw) (-cw) rs cs lx
    else nest cw (-rw) rs cs lx
  | (Left i)::lx ->
      if i == 0 then nest rw cw rs cs lx
      else if i == 90  then  nest cw (-rw) rs cs lx
      else if i == 180 then nest (-rw) (-cw) rs cs lx
      else nest (-cw) rw rs cs lx
  | (Forward i)::lx ->
    nest rw cw (rs + rw*i) (cs + cw*i) lx
  in nest 1 10 0 0 instruction
;;

  55652


let test = [Forward 10; North 3; Forward 7; Right 90; Forward 11];;
