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


let data = read_file "input_24";;
let data_t = read_file "input_24_t";;

type  position = {x: int; y: int};;

let parse_tile line =
  let l = String.length line in

  let rec nest i pp =
  if i == l then pp
  else if (line.[i] == 'e')
  then nest (i+1) {x=pp.x + 2; y=pp.y}
  else if  (line.[i] == 'w')
  then nest (i+1) {x=pp.x-2; y=pp.y}
  else if (line.[i] == 'n')
  then (
    if (line.[i+1] == 'e')
    then nest (i+2) {x=pp.x + 1; y=pp.y+1}
    else nest (i+2) {x=pp.x - 1; y=pp.y+1}
  ) else (
    if (line.[i+1] == 'e')
    then nest (i+2) {x=pp.x + 1; y=pp.y-1}
    else nest (i+2) {x=pp.x - 1; y=pp.y-1}
    )
  in nest 0 {x=0; y=0}
;;


List.sort (fun x y -> if x.x != y.x then (x.x - y.x) else (x.y - y.y)) lst1;;



let solve_1 lines =
  let lst1 = List.map (parse_tile) lines in
  let lst2 = List.sort (fun x y -> if x.x != y.x
    then (x.x - y.x) else (x.y - y.y)) lst1 in

  let rec nest mem c = function
  | [] -> mem
  | x::[] -> (x, c mod 2)::mem
  | x::y::lx -> if (x.x == y.x) && (x.y == y.y)
    then nest mem (c+1) (y::lx)
    else nest ((x, c mod 2)::mem) 0 (y::lx)
    in

  let lst3 = nest [] 0 lst2 in

  List.fold_left (fun (m0, m1) (x, v) ->
    if v == 0 then (x::m0, m1) else (m0, x::m1)
  ) ([], []) lst3
;;

let (l1, l2) = solve_1 data_t;;
Printf.printf "Test: \n\tBlack:\t%d\n\tWhite:\t%d\n" (List.length l1) (List.length l2);;


let (l1, l2) = solve_1 data;;
Printf.printf "Input: \n\tBlack:\t%d\n\tWhite:\t%d\n" (List.length l1) (List.length l2);;


let get_neighborhood pp =
 let rec nest mem = function
 | [] -> mem
 | (dx, dy)::lx -> nest ({x=pp.x+dx; y=pp.y+dy}::mem) lx
 in nest [] [(2, 0); (-2, 0);(1, 1); (1,-1);(-1, 1); (-1,-1)]
 ;;

let count_black pt lst_blk =
  let neig = get_neighborhood pt in
  List.fold_left (fun mem x ->
    if (List.fold_left (fun tf y ->
      tf ||  ((y.x == x.x) && (y.y == x.y))
      ) false lst_blk)
    then mem+1 else mem
    ) 0 neig
;;

(*Remove duplicates *)
let clean_list lst =
  let lst2 = List.sort (fun x y -> if x.x != y.x
    then (x.x - y.x) else (x.y - y.y)) lst in

  let rec nest mem = function
  | [] -> mem
  | x::[] -> x::mem
  | x::y::lx -> if (x.x==y.x) && (x.y==y.y)
    then nest mem (y::lx)
    else nest (x::mem) (y::lx)
  in nest [] lst2
;;

let purge_list lst lnot =

  List.fold_left (fun mem x ->
    if (List.fold_left (fun tf y ->
      tf || ((x.x==y.x) && (x.y==y.y))) false lnot)
    then mem else x::mem
    ) [] lst
;;

let solve_2 data imax =
  let (l_black, _) = solve_1 data in

  let rec nest i l_black =
  Printf.printf "Day %d: \t%d" i (List.length l_black);
  print_newline ();
  if i == imax
  then ()
  else (
    let l_new = List.flatten (List.map (get_neighborhood) l_black) in
    let l_new1 = clean_list l_new in
    let l_new2 = purge_list l_new1 l_black in

    let l_wb = List.fold_left (fun mem x ->
      let c = count_black x l_black in
      if (c == 2) then x::mem else mem
      ) [] l_new2
    and l_bb = List.fold_left (fun mem x ->
      let c = count_black x l_black in
      if (c > 2) || (c==0) then mem else x::mem
      ) [] l_black in
    nest (i+1) (l_bb@l_wb))
  in nest 0 l_black
  ;;
