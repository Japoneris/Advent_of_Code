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



type position = {x:int; y:int; z:int};;

let parse_line x =
  let a::b::c::[] = String.split_on_char ',' x in
  {x=int_of_string a; y=int_of_string b; z=int_of_string c}
;;


let parse_scanner lst =
  let rec nest mem buff = function
  | [] -> buff::mem
  | x::lx -> if String.contains x 's'
    then nest mem buff lx
    else if String.length x < 3
    then nest (buff::mem) [] lx
    else nest mem ((parse_line x)::buff) lx
  in nest [] [] lst
;;


let mirror_x lst =
  List.map (fun p -> {x=(-p.x); y=p.y; z=p.z}) lst
;;

let mirror_y lst =
  List.map (fun p -> {x=p.x; y=(-p.y); z=p.z}) lst
;;

let mirror_z lst =
  List.map (fun p -> {x=p.x; y=p.y; z=(-p.z)}) lst
;;


let flip_xzy lst = List.map (fun p -> {x=p.x; y=p.z; z=p.y}) lst;;
let flip_yxz lst = List.map (fun p -> {x=p.y; y=p.x; z=p.z}) lst;;
let flip_yzx lst = List.map (fun p -> {x=p.y; y=p.z; z=p.x}) lst;;
let flip_zyx lst = List.map (fun p -> {x=p.z; y=p.y; z=p.x}) lst;;
let flip_zxy lst = List.map (fun p -> {x=p.z; y=p.x; z=p.y}) lst;;


let generate_all_possibilities lst =
  let rec nest mem = function
  | [] -> mem
  | fx::lfx -> nest ((List.map (fx) mem) @ mem) lfx
  in
  let lll = nest [lst] (mirror_x::mirror_y::mirror_z::[]) in
  (lll @ (List.map (flip_xzy) lll) @
  (List.map (flip_yxz) lll) @ (List.map (flip_yzx) lll) @
  (List.map (flip_zyx) lll) @ (List.map (flip_zxy) lll)
  )
;;


(*Count the number of similar items*)
let check_length l0 l1 =
  let rec nest mem = function
  | (_, []) | ([], _) -> mem
  | (x::lx, y::ly) ->
    if x == y
    then nest (mem+1) (lx, ly)
    else if x > y
    then nest mem (x::lx, ly)
    else nest mem (lx, y::ly)
  in nest 0 (l0, l1)
;;


(*Check all possible delta between list lx and list ly*)
let get_all_dx lx ly =
  let rec nest mem = function
  | ([], []) | ([], _) -> mem
  | (x::ll, []) -> nest mem (ll, ly)
  | (x::llx, y::lly) -> nest (y-x::mem) (x::llx, lly)
  in

  let rec clean_list mem = function
  | [] -> mem
  | x::[] -> x::mem
  | x::y::lst -> if x == y then clean_list mem (x::lst)
    else clean_list (x::mem) (y::lst)
  in
  let la = nest [] (lx, ly) in
  let lb = List.sort (-) la in
  clean_list [] lb
;;


(*fx: fun v -> v.y*)

let filter_items l0 ll1 =
  let rec nest mem = function
  | ([], []) | (_, []) | ([], _) -> mem
  | (x::lx, (v, y)::ly) ->
    if y == x
    then nest (v::mem) (x::lx, ly) (*Keep the x beacause maybe multiple position for y*)
    else if x < y
    then nest mem (lx, (v, y)::ly)
    else nest mem (x::lx, ly)
  in nest [] (l0, ll1)
;;


let check_intersection sc0 sc1 fx =
  let l0 = List.sort (-) (List.map (fx) sc0) in
  let l1 = List.sort (-) (List.map (fx) sc1) in
  let dd_lst = get_all_dx l0 l1 in

  let l1b = List.map (fun x -> (x, fx x)) sc1 in
  let ll1 = List.sort (fun (_, a) (_, b) -> a - b) l1b in

  let rec nest mem = function
  | [] -> mem
  | dd::ldd -> (
    let lll1 = List.map (fun (p, v) -> (p, v - dd)) ll1 in
    let lll2 = filter_items l0 lll1 in
    if List.length lll2 >= 12
    then nest ((dd, lll2)::mem) ldd
    else nest mem ldd
    )
  in nest []  dd_lst
;;


let is_match_v2 sc0 sc1 =
  let vx = check_intersection sc0 sc1 (fun v -> v.x ) in
  if List.length vx > 0
  then (
    let (dx, sc1x)::_ = vx in
    Printf.printf "X: %d match => %d\n" (List.length vx) (List.length sc1x);
    let vy = check_intersection sc0 sc1x (fun v -> v.y ) in
    if List.length vy > 0
    then (
      let (dy, sc1y)::_ = vy in
      Printf.printf "\tY: %d match => %d\n" (List.length vy) (List.length sc1y);
      let vz = check_intersection sc0 sc1y (fun v -> v.z ) in
      if List.length vz > 0
      then (
        let (dz, sc1z)::_ = vz in
        Printf.printf "\t\tZ: %d match ==> %d\n" (List.length vz) (List.length sc1z);
          [(dx, dy, dz)]
        )
      else []
      )
    else []
    )
  else []
;;

let verify sc0 sc1 dx dy dz =
  let cc0 = merge_blocks sc1 sc0 (-dx) (-dy) (-dz) in
  let ccc0 = List.fold_left (fun mem v ->
    if (abs v.x > 1000) || (abs v.y > 1000) || (abs v.z > 1000)
    then mem else v::mem ) [] cc0 in
  List.length ccc0 == List.length sc1
;;

let is_any_match_v2 sc0 sc1 =
  let tiles_1 = generate_all_possibilities sc1 in
  List.fold_left (fun mem x ->
    let lll = is_match_v2 sc0 x in
    match lll with
    | [] -> mem
    | (dx, dy, dz)::_ ->
      if verify sc0 x dx dy dz
      then ((merge_blocks sc0 x dx dy dz)::mem)
      else (
        Printf.printf "UNVALID\n\n";
        mem)) [] tiles_1
;;



let solve_all_v2 scan_lst =
  let rec nest = function
  | [] -> []
  | sc::[] -> sc
  | sc0::left -> (
    let (left1, sc1) = List.fold_left (fun (lft, mem) sci ->
      let lll = is_any_match_v2 mem sci in

      match lll with
      | []-> (sci::lft, mem)
      | scj::_ -> (lft, scj)
      ) ([], sc0) left in

      Printf.printf "Left %d items, and get %d sensors" (List.length left1) (List.length sc1);
      print_newline ();
    nest (sc1::left1)
    )
  in nest scan_lst
;;





(*return (true, [x, y,z] relatively to sc0) if okay*)


let cmp_blocks a b =
  if a.x > b.x then 1
    else if a.x < b.x then -1
    else if a.y > b.y then 1
    else if a.y < b.y then -1
    else a.z - b.z
;;

let sort_blocks lst = List.sort (cmp_blocks) lst;;

let filter_blocks lst =
  let rec nest mem = function
  | [] -> mem
  | x::[] -> x::mem
  | x::y::lll -> if cmp_blocks x y == 0
    then nest mem (x::lll)
    else nest (x::mem) (y::lll)
  in nest [] lst
;;


let merge_blocks c0 c1 dx dy dz =
  (*Translate blocks c1*)
  let cc1 = List.map (fun v -> {x=v.x-dx; y=v.y-dy; z=v.z-dz}) c1 in
  let lst = c0 @ cc1 in
  filter_blocks (sort_blocks lst)
;;



let lst = read_file "input_19.txt";;
let scanner_lst = parse_scanner lst;;
let results = solve_all_v2 scanner_lst;;

let sc0 = List.hd scanner_lst;;
List.map (fun sci -> is_any_match sc0 sci) (List.tl scanner_lst);;
List.fold_left (fun mem sci ->
  let lll = is_any_match mem sci in
  match lll with
  | []-> mem
  | (scj, [((dx, _)::_, (dy,_)::_, (dz, _)::_)])::_ ->
    merge_blocks mem scj dx dy dz
  ) sc0 (List.tl scanner_lst)
;;



(*719 too high*)
(*Bug with reduction*)
(*368 too high*)
(*Bug might be wrong overlap*)

(*335*)

(*Part 2: measure scanner distances*)
(*From part 1 results, get back scanner location*)

let is_any_match_v2 sc0 sc1 =
  let tiles_1 = generate_all_possibilities sc1 in
  List.fold_left (fun mem x ->
    let lll = is_match_v2 sc0 x in
    match lll with
    | [] -> mem
    | (dx, dy, dz)::_ ->
      if verify sc0 x dx dy dz
      then ((merge_blocks sc0 x dx dy dz)::mem)
      else (
        Printf.printf "UNVALID\n\n";
        mem)) [] tiles_1
;;



let get_scanner_positions sc lst_probes =
  let tiles = generate_all_possibilities sc in
  List.fold_left (fun (tf, px, py, pz) x ->
    if tf then (tf, px, py, pz)
    else
    let lll = is_match_v2 lst_probes x in
    match lll with
    | [] -> (tf, px, py, pz)
    | (dx, dy, dz)::_ -> (true, dx, dy, dz)
      ) (false, 0, 0, 0) tiles
;;


let get_max_dist lst =
  let lst1 = List.map (fun (a,b,c,d) -> (b,c,d)) lst in
  let rec nest mem = function
  | [] | _::[] -> mem
  | (a,b,c)::lx ->

    let v = List.fold_left (fun mem (ai, bi, ci) ->
      let vi = abs (ai - a) + abs (bi - b) + abs (ci - c) in
      max mem vi) mem lx in
    nest v lx
  in nest 0 lst1
;;

let scanner_positions = List.map (fun sc -> get_scanner_positions sc results) scanner_lst;;
get_max_dist scanner_positions;;
