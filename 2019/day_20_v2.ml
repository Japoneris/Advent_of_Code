let read_input_data path ly =
  let ic = open_in path in
  let mat = Array.init ly (fun _ ->
    let line = input_line ic in
    let l = String.length line in
    Array.init l (fun i ->
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


let puzzle = read_input_data "input_20" 135;;

let data_test = "                   A
                   A
  #################.#############
  #.#...#...................#.#.#
  #.#.#.###.###.###.#########.#.#
  #.#.#.......#...#.....#.#.#...#
  #.#########.###.#####.#.#.###.#
  #.............#.#.....#.......#
  ###.###########.###.#####.#.#.#
  #.....#        A   C    #.#.#.#
  #######        S   P    #####.#
  #.#...#                 #......VT
  #.#.#.#                 #.#####
  #...#.#               YN....#.#
  #.###.#                 #####.#
DI....#.#                 #.....#
  #####.#                 #.###.#
ZZ......#               QG....#..AS
  ###.###                 #######
JO..#.#.#                 #.....#
  #.#.#.#                 ###.#.#
  #...#..DI             BU....#..LF
  #####.#                 #.#####
YN......#               VT..#....QG
  #.###.#                 #.###.#
  #.#...#                 #.....#
  ###.###    J L     J    #.#.###
  #.....#    O F     P    #.#...#
  #.###.#####.#.#####.#####.###.#
  #...#.#.#...#.....#.....#.#...#
  #.#####.###.###.#.#.#########.#
  #...#.#.....#...#.#.#.#.....#.#
  #.###.#####.###.###.#.#.#######
  #.#.........#...#.............#
  #########.###.###.#############
           B   J   C
           U   P   P               "
;;


let data_test = "             Z L X W       C
             Z P Q B       K
  ###########.#.#.#.#######.###############
  #...#.......#.#.......#.#.......#.#.#...#
  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###
  #.#...#.#.#...#.#.#...#...#...#.#.......#
  #.###.#######.###.###.#.###.###.#.#######
  #...#.......#.#...#...#.............#...#
  #.#########.#######.#.#######.#######.###
  #...#.#    F       R I       Z    #.#.#.#
  #.###.#    D       E C       H    #.#.#.#
  #.#...#                           #...#.#
  #.###.#                           #.###.#
  #.#....OA                       WB..#.#..ZH
  #.###.#                           #.#.#.#
CJ......#                           #.....#
  #######                           #######
  #.#....CK                         #......IC
  #.###.#                           #.###.#
  #.....#                           #...#.#
  ###.###                           #.#.#.#
XF....#.#                         RF..#.#.#
  #####.#                           #######
  #......CJ                       NM..#...#
  ###.#.#                           #.###.#
RE....#.#                           #......RF
  ###.###        X   X       L      #.#.#.#
  #.....#        F   Q       P      #.#.#.#
  ###.###########.###.#######.#########.###
  #.....#...#.....#.......#...#.....#.#...#
  #####.#.###.#######.#######.###.###.#.#.#
  #.......#.......#.#.#.#.#...#...#...#.#.#
  #####.###.#####.#.#.#.#.###.###.#.###.###
  #.......#.....#.#...#...............#...#
  #############.#.#.###.###################
               A O F   N
               A A D   M                    ."
;;
let arr_test = Array.of_list (String.split_on_char '\n' data_test);;
let l = String.length arr_test.(10) + 2 ;;
let puzzle_test = Array.map (fun line ->
  let row = Array.make l ' ' in
  String.iteri (fun i c -> row.(i) <- c) line;
  row) arr_test
;;


type direction = Left | Right | Top | Down;;
type inout = In | Out;;
type position = {x:int; y:int};;
type pos_dist = {p: position; depth: int};;
type teleporter = {p: position; d: direction; name: char list; k: inout};;

let is_char c =
  let cv = int_of_char c in
  (65 <= cv) && (cv <= 90)
;;

let get_teleporter_out mat =
  let lx = Array.length mat.(0)
  and ly = Array.length mat in

  let rec nest_v mem i j =
  if i == ly then mem
  else  (
    let cv = int_of_char mat.(i).(j) in
    if  (65 <= cv) && (cv <= 90)
    then
      if j == 1 then
      nest_v ({p={x=j+1; y=i}; d=Left; name=[mat.(i).(j-1); mat.(i).(j)]; k=Out}::mem) (i+1) j
      else
      nest_v ({p={x=j-1; y=i}; d=Right; name=[mat.(i).(j); mat.(i).(j+1)]; k=Out}::mem) (i+1) j
    else nest_v mem (i+1) j
    ) in

  let rec nest_h mem i j =
  if j == lx then mem
  else  (
    let cv = int_of_char mat.(i).(j) in
    if  (65 <= cv) && (cv <= 90)
    then
      if i == 1 then
      nest_h ({p={x=j; y=i+1}; d=Top; name=[mat.(i-1).(j); mat.(i).(j)]; k=Out}::mem) i (j+1)
      else
      nest_h ({p={x=j; y=i-1}; d=Down; name=[mat.(i).(j); mat.(i+1).(j)]; k=Out}::mem) i (j+1)
    else nest_h mem i (j+1)
    ) in

  (nest_v [] 0 1) @ (nest_v [] 0 (lx-2)) @ (nest_h [] 1 0) @ (nest_h [] (ly-2) 0)
;;



let get_teleporter_in mat =
  let lx = Array.length mat.(0)
  and ly = Array.length mat in

  (*Get where is the middle inside*)
  let test c0 c1 =
  if (c0 == ' ') && (c1 == '#')
  then true
  else (is_char c0) && (c1 == '.')
  in

  let rec search_border i j di dj =
  if test mat.(i).(j) mat.(i+di).(j+dj)
  then if di == 0 then j else i
  else search_border (i+di) (j+dj) di dj
  in

  let bord_down   = search_border (ly/2) (lx/2) 1 0
  and bord_top    = search_border (ly/2) (lx/2) (-1) 0
  and bord_left   = search_border (ly/2) (lx/2) 0 (-1)
  and bord_right  = search_border (ly/2) (lx/2) 0 1
  in

  let rec nest_v mem i j =
  if i == bord_down then mem
  else  (
    if is_char mat.(i).(j)
    then
      if j < (lx/2)
      then nest_v ({p={x=j-1; y=i}; d=Left; name=[mat.(i).(j); mat.(i).(j+1)]; k=In}::mem) (i+1) j
      else nest_v ({p={x=j+1; y=i}; d=Right; name=[mat.(i).(j-1); mat.(i).(j)]; k=In}::mem) (i+1) j
    else nest_v mem (i+1) j
    ) in

  let rec nest_h mem i j =
  if j == bord_right then mem
  else  (
    if is_char mat.(i).(j)
    then
      if i < (ly/2)
      then nest_h ({p={x=j; y=i-1}; d=Top; name=[mat.(i).(j); mat.(i+1).(j)]; k=In}::mem) i (j+1)
      else nest_h ({p={x=j; y=i+1}; d=Down; name=[mat.(i-1).(j); mat.(i).(j)]; k=In}::mem) i (j+1)
    else nest_h mem i (j+1)
    ) in

    (nest_v [] 0 bord_right) @ (nest_v [] 0 bord_left) @ (nest_h [] bord_top 0) @ (nest_h [] bord_down 0)
  ;;



let copy_puzzle puzzle =
  Array.map (fun row -> Array.copy row) puzzle
;;

let get_next_position pt =
  List.map (fun (a, b) -> {p={x=pt.p.x+a; y=pt.p.y+b}; depth=pt.depth+1}) [(0, 1); (0, -1); (1, 0); (-1, 0)]
;;


let assign_teleporter lst_tele p =

  let (_, pbest) = List.fold_left (fun (d, t_mem) pi ->
  let di = abs (p.x - pi.p.x) + abs (p.y - pi.p.y) in
  if di < d then (di, pi) else (d, t_mem)
  ) (10000, {p={x=0; y=0}; d=Down; name=[' '; ' ']; k=In})  lst_tele in pbest
;;

let resolve_names teleporters thing_lst =
  List.map (fun x -> (assign_teleporter teleporters x.p, x.depth)) thing_lst
;;


let get_dist puzzle pos =
  let pzl = copy_puzzle puzzle in

  let rec nest mem = function
  | [] -> mem
  | lst -> (

    (*Near a teleporter*)
    let endings = List.fold_left (fun lx pp ->
      let p = pp.p in
      if (pzl.(p.y).(p.x) == '.') && ((is_char pzl.(p.y).(p.x+1)) || (is_char pzl.(p.y).(p.x-1)) || (is_char pzl.(p.y-1).(p.x)) || (is_char pzl.(p.y+1).(p.x)))
      then pp::lx
      else lx
      ) [] lst in


    let candidates = List.fold_left (fun lx pp ->
      let p = pp.p in
      if pzl.(p.y).(p.x) == '.'
      then (pzl.(p.y).(p.x) <- '#'; pp::lx)
      else lx ) [] lst in

    nest (endings @ mem) (List.flatten (List.map (get_next_position) candidates))
  )
  in nest [] [{p=pos; depth=0}]
;;

let extract_distance puzzle tele =
  List.map (fun pp ->
    let i = pp.name in
    get_dist puzzle pp.p) tele
;;




let teleporters  = (get_teleporter_in puzzle_test) @ (get_teleporter_out puzzle_test);;

let d_in_to = extract_distance puzzle_test teleporters;;
let d_tel = List.map (fun x -> resolve_names teleporters x) d_in_to;;



let puzzle = read_input_data "input_20" 135;;
let teleporters  = (get_teleporter_in puzzle) @ (get_teleporter_out puzzle);;
let d_in_to = extract_distance puzzle teleporters;;
let d_tel = List.map (fun x -> resolve_names teleporters x) d_in_to;;

(*Take this output to python notebook*)
List.iter (fun row ->
  List.iter (fun (x, d) ->
    let lb = match x.k with
    | In -> "in"
    | Out -> "out" in

    let ci::cj::lx = x.name in
    Printf.printf "%c%c:%s:%d " ci cj lb d
    ) (List.rev row );
  print_newline ()
  ) d_tel;;
