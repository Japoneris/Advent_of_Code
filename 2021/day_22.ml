


type position = {x:int; y:int; z:int};;
type area = {pmin: position; pmax: position};;

type instruction =
  | On of area
  | Off of area
;;

let get_val stri =
  let _::b::[] = String.split_on_char '=' stri in
  let b0::_::b1::[] = String.split_on_char '.' b in
  (int_of_string b0, int_of_string b1)
;;


let parse_line stri =
  let a::b = String.split_on_char ' ' stri in
  let x::y::z::[] = String.split_on_char ',' stri in
  let (x0, x1) = get_val x
  and (y0, y1) = get_val y
  and (z0, z1) = get_val z in

  if String.length a == 2
  then On  {pmin={x=x0; y=y0; z=z0}; pmax={x=x1; y=y1; z=z1}}
  else Off {pmin={x=x0; y=y0; z=z0}; pmax={x=x1; y=y1; z=z1}}
;;


let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    nest (parse_line line::lst)
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    (List.rev lst)
  in nest []
;;


let is_valid_input trx =
  let check px =
    (px.x <= 50) && (px.x >= -50) && (px.y <= 50) && (px.y >= -50) && (px.z <= 50) && (px.z >= -50)
  in
  match trx with
  | On area | Off area -> (check area.pmin) && (check area.pmax)
;;








let lst = read_file "input_22.txt";;
let lst = read_file "input_22_test";;
let lst_1 = List.rev (List.fold_left (fun mem x -> if is_valid_input x then x::mem else mem) [] lst);;


let results = process_instruction lst_1;;
List.length results;;


(*TODO: need an efficient data structure
Use area: replace area when turned off
*)


let turn_cube_on_v2 area = {pmin=area.pmin; pmax=area.pmax};;

let is_overlap area0 area1 =
  (*
  0: |pmin------pmax|
  1:                 |pmin-----------pmax|

  0: |pmin------pmax|
  1:         |pmin-----------pmax|

  *)
  if (
    (area0.pmax.x <= area1.pmin.x) || (area1.pmax.x <= area0.pmin.x) ||
    (area0.pmax.y <= area1.pmin.y) || (area1.pmax.y <= area0.pmin.y) ||
    (area0.pmax.z <= area1.pmin.z) || (area1.pmax.z <= area0.pmin.z)
    )
  then false
  else true
;;


let filter_number lst =
  let rec nest mem = function
  | [] -> List.rev mem
  | x::[] -> nest (x::mem) []
  | x::y::lx ->
    if x == y
    then nest mem (x::lx)
    else nest (x::mem) (y::lx)
  in nest [] (List.sort (-) lst)
;;

let get_mid_block area0 area1 =
  let (x0, x1) = (max area0.pmin.x area1.pmin.x, min area0.pmax.x area1.pmax.x)
  and (y0, y1) = (max area0.pmin.y area1.pmin.y, min area0.pmax.y area1.pmax.y)
  and (z0, z1) = (max area0.pmin.z area1.pmin.z, min area0.pmax.z area1.pmax.z)
  in
  {pmin={x=x0; y=y0; z=z0}; pmax={x=x1; y=y1; z=z1}}
;;



(*area1: subblocks with limits near area0*)
(* (true, xx) if xx overlap the other block *)
let gen_sublocks area0 area1 =
  (*sorted by increasing order*)
  (*permet de trouver les éléments qui intersectent*)
  let lsx = filter_number [area0.pmin.x; area1.pmin.x; area1.pmax.x; area0.pmax.x]
  and lsy = filter_number [area0.pmin.y; area1.pmin.y; area1.pmax.y; area0.pmax.y]
  and lsz = filter_number [area0.pmin.z; area1.pmin.z; area1.pmax.z; area0.pmax.z]
  in


  let rec nest mem = function
  | ([], _, _) -> mem
  | (x::[], _, _) -> mem
  | (x0::lx, [], _) -> nest mem (lx, lsy, lsz)
  | (x0::lx, y::[], _) -> nest mem (lx, lsy, lsz)
  | (lx, y::ly, []) -> nest mem (lx, ly, lsz) (*Normalement, jamaos atteint*)
  | (lx, y::ly, z::[]) -> nest mem (lx, ly, lsz)
  (*2 elements de chaque pour faire le job*)
  | (x0::x1::lx, y0::y1::ly, z0::z1::lz) ->
    nest ({pmin={x=x0; y=y0; z=z0}; pmax={x=x1; y=y1; z=z1}}::mem) (x0::x1::lx, y0::y1::ly, z1::lz)
  in

  let lst_blocks = nest [] (lsx, lsy, lsz) in

  List.map (fun x -> (is_overlap x area1, x)) lst_blocks
;;



let turn_cube_off_v2 area0 lst =

  let rec nest mem = function
  | [] -> mem
  | px::lx ->
    if is_overlap area0 px
    then (
      let mid = get_mid_block px area0 in
      Printf.printf "\tOverlap with (x: %d..%d, y: %d..%d, z: %d..%d)\n" px.pmin.x px.pmax.x px.pmin.y px.pmax.y px.pmin.z px.pmax.z;
      Printf.printf "\tMidblock: (x: %d..%d, y: %d..%d, z: %d..%d)\n" mid.pmin.x mid.pmax.x mid.pmin.y mid.pmax.y mid.pmin.z mid.pmax.z;
      let new_blocks = gen_sublocks px mid in
      let filt_blocks = List.fold_left (fun mem (tf, blk) ->
        if tf
        then mem
        else blk::mem) [] new_blocks in
      Printf.printf "\t Generated %d-> %d blocks\n" (List.length new_blocks) (List.length filt_blocks);
      nest (filt_blocks @ mem) lx
      )
    else nest (px::mem) lx
  in nest [] lst
;;


let process_instruction lst =
  let rec nest mem = function
  | [] -> mem
  | (On area)::linst -> (
    Printf.printf "Turn on (x: %d..%d, y: %d..%d, z: %d..%d) \t(%d elements)\n" area.pmin.x area.pmax.x area.pmin.y area.pmax.y area.pmin.z area.pmax.z (List.length mem);
    nest ((turn_cube_on_v2 area) :: mem) linst
    )
  | (Off area)::linst -> (
    Printf.printf "Turn off: (x: %d..%d, y: %d..%d, z: %d..%d)\n" area.pmin.x area.pmax.x area.pmin.y area.pmax.y area.pmin.z area.pmax.z;
    print_newline ();
    nest (turn_cube_off_v2 area mem) linst
    )
  in nest [] lst
;;


(*To be easy for border*)
let increase_by_one pos =  {x=(pos.x+1); y=(pos.y+1); z=(pos.z+1)} ;;



(*Split x into smaller pieces until not possible to update*)
(*Remove duplicatas *)
let split x y =
  if is_overlap x y  (*y: main block*)
  then (
    let mid = get_mid_block x y in
    let new_blocks = gen_sublocks x mid in
    let filt_blocks = List.fold_left (fun mem (tf, blk) -> if tf then mem else blk::mem) [] new_blocks in
    (*Do not keep the middle block, so the overlap of x / y is zero after *)

    let new_blocks1 = gen_sublocks y mid in
    let filt_blocks1 = List.fold_left (fun mem (tf, blk) -> blk::mem) [] new_blocks1 in
    (true, filt_blocks, filt_blocks1)
    )
  else (false, [x], [y])
;;


let insert b0 lst =
  let rec nest memy = function
  | ([], []) -> memy
  | (ly, []) -> ly @ memy (*Lorsque tous les x ont été processé, alors on accepte l'ensemble*)
  | ([], x::lx) -> nest [] (x::memy, lx) (*Lorsque x confronté à tous les y, alors x est accepté*)
  | (y::ly, x::lx) -> (
    let (tf, blx, bly) = split x y in (*Split met le morceau qui se recouvre dans bly*)
    if tf
    then nest memy (bly @ ly, blx @ lx)
    else nest (y::memy) (ly, x::lx) (*Confronte x avec tous les y existants*)
    )
  in nest [] (lst, [b0])
;;

let merge_blocks lst =
  (*maintain a list that does not overlap*)
  let rec nest mem = function
  | [] -> mem
  | x::lx -> nest (insert x mem) lx
  in nest [] lst
;;

let is_any_overlap lst =
  let check x lx =  List.fold_left (fun mem y -> if mem then mem else is_overlap x y) false lx in

  let rec nest mem = function
  | x::lx -> if check x lx then nest (x::mem) lx else nest mem lx
  | [] -> mem
  in nest [] lst
;;

let merge_safe lst =
  let rec nest mem =
    let l1 = merge_blocks mem in
    let l2 = is_any_overlap l1 in
    Printf.printf "New step: %d vs %d" (List.length l1) (List.length l2);
    print_newline ();

    if List.length l2 != 0
    then nest l1
    else l1
  in nest lst
;;


let measure_area blk =
  let dx = blk.pmax.x - blk.pmin.x
  and dy = blk.pmax.y - blk.pmin.y
  and dz = blk.pmax.z - blk.pmin.z
  in dx * dy * dz
;;


(*RUN*)

let update_input lst =
  List.map (fun instr ->
    match instr with
    | On area -> On {pmin=area.pmin; pmax=(increase_by_one area.pmax)}
    | Off area -> Off {pmin=area.pmin; pmax=(increase_by_one area.pmax)}
    ) lst
;;



let lst = read_file "input_22.txt";;
let lst = read_file "input_22_test";;
let lst_1 = List.rev (List.fold_left (fun mem x -> if is_valid_input x then x::mem else mem) [] lst);;

let lst_2 = update_input lst_1;;
let results = process_instruction lst_2;;
let results_1 = merge_safe results;;
List.fold_left (fun mem x -> mem  + (measure_area x)) 0 results_1;;

(*True 590784 for test *)




(*Part 2 *)

let lst = read_file "input_22_test_1";;
let lst = read_file "input_22.txt";;
let lst_2 = update_input lst;;
let results = process_instruction lst_2;;
let results_1 = merge_safe results;;
List.fold_left (fun mem x -> mem  + (measure_area x)) 0 results_1;;
