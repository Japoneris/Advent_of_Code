type direction =
  | Left
  | Right
  | Up
  | Down
;;

let char_to_dir c =
  if c == 'L' then Left
  else if c == 'R' then Right
  else if c == 'D' then Down
  else Up
;;

let scanint x = Scanf.sscanf x "%c%d " (fun x y -> (char_to_dir x, y));;

let transform inp =
  let sk = String.split_on_char ',' inp in
  List.map (scanint) sk
;;

let read_input_data () =
  let ic = open_in "input_03" in
  let line_1 = input_line ic in
  let line_2 = input_line ic in
  close_in_noerr ic;
  (transform line_1, transform line_2)
  ;;

type segments = {sx: int; sy: int; dx: int; dy: int};;
type location = {x: int; y: int};;

let update_position dir v p =
  match dir with
  | Left -> {x = p.x - v; y=p.y}
  | Right -> {x = p.x + v; y=p.y}
  | Up -> {x = p.x; y=p.y - v}
  | Down -> {x = p.x; y=p.y + v}
;;

let make_segment p0 p1 =
  {sx = min p0.x p1.x; sy=min p0.y p1.y;
    dx = abs (p0.x - p1.x); dy = abs (p0.y - p1.y)}
;;

(*Assume x is horizontal segment*)
let get_interset x y =
  if (y.sx > x.sx) && (y.sx < x.sx + x.dx)
  then (
    if (x.sy > y.sy) && (x.sy < y.sy + y.dy)
    then (true, {x=x.sy; y=y.sx})
    else (false, {x=0; y=0})
    )
  else (false, {x=0; y=0})
;;

let get_intersect_h x lst =
  List.fold_left (fun mem y ->
    let (tf, p) = get_interset x y in
    if tf then p::mem else mem) [] lst
;;

let get_intersect_tot lh lv =
  List.fold_left (fun mem x -> (get_intersect_h x lv)@mem) [] lh
;;


let solve data =
  let rec nest mem p = function
  | [] -> mem
  | (dir, v)::lst -> (
    let p1 = update_position dir v p in
    let seg = make_segment p p1 in
    nest (seg::mem) p1 lst
    )
  in
  let lst_segments = nest [] {x=0; y=0} data  in

  let (lst_h, lst_v) = List.fold_left (fun (lh, lv) seg ->
    if seg.dx == 0
    then (lh, seg::lv)
    else (seg::lh, lv)
    ) ([], []) lst_segments in
  (lst_h, lst_v)
;;



let (data_1, data_2) = read_input_data ();;

let data_1 = transform "R8,U5,L5,D3";;
let data_2 = transform "U7,R6,D4,L4";;

(*159*)
let data_1 = transform "R75,D30,R83,U83,L12,D49,R71,U7,L72";;
let data_2 = transform "U62,R66,U55,R34,D71,R55,D58,R83";;


let data_1 = transform "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51";;
let data_2 = transform "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7";;


let (l11, l12) = solve data_1;;
let (l21, l22) = solve data_2;;

let res_1 = get_intersect_tot l11 l22;;
let res_2 = get_intersect_tot l21 l12;;

List.fold_left (fun mem x ->
  let d = ((abs x.x) + (abs x.y)) in
  if d == 0 then mem else min mem d
  ) 1000000000000 (res_1@res_2)
;;



(*Part 2*)

let check_interset x y =
  if (y.sx > x.sx) && (y.sx < x.sx + x.dx)
  then (
    if (x.sy > y.sy) && (x.sy < y.sy + y.dy)
    then (true, {x=y.sx; y=x.sy})
    else (false, {x=0; y=0})
    )
  else (false, {x=0; y=0})
;;

let get_interset x y =
  let (tf, r) = check_interset x y in
  if tf then (true, r)
  else (
    let (tf, r) = check_interset y x in
    if tf then (true, r)
    else (false, {x=0; y=0})
    )
;;


let data_1 = transform "R8,U5,L5,D3";;
let data_2 = transform "U7,R6,D4,L4";;

let solve_p2 data_1 data_2 =

  let measure seg p0 pn =
    let dx = seg.dx - abs (p0.x - pn.x)
    and dy = seg.dy - abs (p0.y - pn.y)
    in dx + dy
  in

  let rec nest c0 c1 p0 p1 seg0 mem = function
  | ([], []) -> mem
  | ((dir_x, x)::lsx, []) ->
    let p00 = update_position dir_x x p0 in
    let seg = (make_segment p0 p00) in
    nest (c0 + seg.dx + seg.dy) 0
        p00 {x=0; y=0}
        seg mem
        (lsx, data_2)

  | (lsx, (dir_y, y)::lsy) ->
    let p11 = update_position dir_y y p1 in
    let seg1 = make_segment p1 p11 in
    let (tf, p1x) = get_interset seg1 seg0 in
    let mem1 = (if tf
    then (

      let m1 = (measure seg0 p0 p1x)
      and m2 = (measure seg1 p11 p1x)
      in
      (c0 - seg0.dx - seg0.dy + c1 + m1 + m2)::mem
      )
    else mem) in
    let c11 = c1 + seg1.dx + seg1.dy in
    nest c0 c11 p0 p11 seg0 mem1 (lsx, lsy)
  in nest 0 0 {x=0;y=0} {x=0;y=0}
    {sx=0;sy=0; dx=0; dy=0} [] (data_1, [])
;;

let result = solve_p2 data_1 data_2;;
List.fold_left (fun mem x -> min mem x) 1000000000 result;;
