
let test_0 = ".#..#
.....
#####
....#
...##";;

let test_1 ="......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####";;

let test_2 = "#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.";;

let test_3 = ".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..";;

let test_4 = ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##";;

let test_5 = ".#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....#...###..
..#.#.....#....##";;

let data_t0 = String.split_on_char '\n' test_0;;
let data_t1 = String.split_on_char '\n' test_1;;
let data_t2 = String.split_on_char '\n' test_2;;
let data_t3 = String.split_on_char '\n' test_3;;
let data_t4 = String.split_on_char '\n' test_4;;
let data_t5 = String.split_on_char '\n' test_5;;
let data = read_input_data "input_10";;

let read_input_data path =
  let ic = open_in path in
  let rec nest lst =
    try
      nest ((input_line ic )::lst)
     with e ->
     close_in_noerr ic;
     lst in
    nest []
;;

type asteroid = {x:int; y:int};;
let get_asteroid_position data =
  let ly = List.length data
  and lx = String.length (List.hd data) in

  (*Extract list of asteroid*)
  let extract_line y line =
    let arr_line = (Array.init lx (fun i -> line.[i])) in
    let (res, _) = Array.fold_left (fun (mem, i) c ->
      if c == '#' then ({x=i; y=y}::mem, i+1) else (mem, i+1)
    ) ([], 0) arr_line in
    res in

  List.flatten (List.mapi (fun i line -> extract_line i line) data)
;;

type line_sight = {dx:int; dy:int};;

let add_ast lst ax =
  let tf = List.fold_left (fun mem ai ->
    if mem
    then check_los ai ax
    else mem
    ) true lst
  in if tf then ax::lst else lst
;;

let check_los ai ax =
  if (ai.dx == ax.dx) && (ai.dx == 0)
  then (ai.dy * ax.dy < 0)
  else if (ai.dy == ax.dy) && (ai.dy == 0)
  then (ai.dx * ax.dx < 0)
  else if (ai.dx * ax.dy == ai.dy * ax.dx) && (ai.dx * ax.dx > 0)
  then false
  else true
;;

let count_line_of_sight a0 asteroids =
  let rec nest mem = function
  | [] -> List.length mem
  | a1::ax ->
    if (a0.x == a1.x) && (a0.y == a1.y)
    then nest mem ax
    else (
      let los = {dx=a1.x - a0.x; dy=a1.y - a0.y} in
      nest (add_ast mem los) ax
      ) in nest [] asteroids
;;



let add_ast_1 (lst, keep) a aref =
  let los = {dx=a.x - aref.x; dy=a.y - aref.y} in

  let rec nest mem = function
  | [] -> (a::mem, keep)
  | ax::lax -> (
    let los_1 = {dx=ax.x - aref.x; dy=ax.y - aref.y} in

    if check_los los los_1 (*True: not on the same line*)
    then nest (ax::mem) lax
    else if (abs los_1.dx + abs los_1.dy) < (abs los.dx + abs los.dy)
    then (lst, a::keep)
    else (a::mem@lax, ax::keep)
    ) in nest [] lst

  ;;



let count_line_of_sight_2 a0 asteroids =
  let rec nest mem = function
  | [] -> mem
  | a1::ax ->
    if (a0.x == a1.x) && (a0.y == a1.y) (*Same asteroid*)
    then nest mem ax
    else (
      nest (add_ast_1 mem a1 a0) ax
      ) in nest ([], [a0]) asteroids
;;


(**)

let solve_p1 data =
  let astero = get_asteroid_position data in
  let (a_best, c) = List.fold_left (fun (mem, ci) ax ->
    let cj = count_line_of_sight ax astero in
    if cj > ci then (ax, cj) else (mem, ci)
  ) ({x=(-1); y=(-1)}, 0) astero in
  (a_best, c)
;;


let (p1, c) = solve_p1 data_t4;;

let pi2 = asin 1.;;

let angle p =
  if p.dx == 0
  then (if p.dy > 0
      then 0.
      else pi2 *. 2.
  )
  else if p.dy == 0
  then (if p.dx > 0
    then pi2
    else pi2*.3.
    )
  else (
    let d = float (p.dx *p.dx + p.dy * p.dy) in
    let c = (float (abs p.dx)) /. (sqrt d) in

    let ang = asin c in
    if (p.dx > 0) && (p.dy > 0) then ang
    else if (p.dx > 0) && (p.dy < 0) then 2. *. pi2 -. ang
    else if (p.dx < 0) && (p.dy < 0) then 2. *. pi2 +. ang
    else (*if (p.dx < 0) && (p.dy > 0) then*) 4. *. pi2 -. ang
    )
;;


let get_angle aster x =
  List.map (fun y -> ( y, angle {dx=y.x - x.x; dy=x.y - y.y})) aster
;;

let sort_circular aster x =
  let l0 = get_angle aster x in
  List.sort (fun (a, a0) (b, b0) -> if a0 > b0 then 1 else (-1)) l0
;;


(*Misunderstanding*)

let solve_p2 data =

  let astero = get_asteroid_position data in
  let (a_best, (row, keep), c) = List.fold_left (fun (mem, (r, k), ci) ax ->
    let (row, keep) = count_line_of_sight_2 ax astero in

    let cj = List.length row in
    if cj > ci
    then (ax, (row, keep), cj)
    else (mem, (r, k),  ci)
  ) ({x=(-1); y=(-1)}, ([], []), 0) astero in
  (a_best, (row, keep), c)
;;


let solve_p3 data =
  let (a1, (r1, k1), c1) = solve_p2 data in
  let arr = Array.of_list (sort_circular r1 a1) in
  Array.iteri (fun  i (x,y) -> Printf.printf "%d:\t %d %d: \t%f\n" i x.x x.y y) arr;
  (arr, a1, c1)
;;

let print_p2 row a =
  let arr = Array.of_list (sort_circular row a) in
  Array.iteri (fun  i (x, y) ->
    let dx = x.x - a.x
    and dy = x.y - a.y in
  Printf.printf "%d:\t %d %d: \t%f \t (%d, %d) = %f\n" i x.x x.y y dx dy (float dx /. float dy)) arr;
;;


let solve_p3 data =
  let astero = get_asteroid_position data in

  let rec nest mem = function
  | [] -> mem
  | x::[] -> x::mem
  | aster_x -> (
    let (a_best, (row, keep), c) = List.fold_left (fun (mem, (r, k), ci) ax ->
      let (row, keep) = count_line_of_sight_2 ax aster_x in
      let cj = List.length row in

      if cj > ci
      then (ax, (row, keep), cj)
      else (mem, (r, k),  ci)
    ) ({x=(-1); y=(-1)}, ([], []), 0) astero in

    Printf.printf "Round: deletion of %d \tfrom (%d %d), left: %d\n" (List.length row) a_best.x a_best.y (List.length keep);
    nest (row @ mem) keep
  )
  in nest [] astero
;;
