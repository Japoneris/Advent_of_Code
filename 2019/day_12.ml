type planet = {x:int; y:int; z:int};;
type velo = {dx: int; dy: int; dz:int};;
type galaxy = {p: planet; v:velo};;

let scan_input x = Scanf.sscanf x "<x=%d, y=%d, z=%d> " (fun x y z -> {x=x;y=y;z=z});;

let read_input_data path =
  let ic = open_in path in
  let lst = List.init 4 (fun _ -> scan_input (input_line ic)) in
  close_in_noerr ic;
  lst
;;




let add_gravity ga gb =
  {dx=ga.dx + gb.dx; dy= ga.dy + gb.dy; dz= ga.dz+gb.dz}
;;

let check v0 v1 =
  if v0 < v1 then (1, -1)
  else if v0 > v1 then (-1, 1)
  else (0, 0)
;;

let gravity_pair a b =
  let (x0, x1) = check a.x b.x
  and (y0, y1) = check a.y b.y
  and (z0, z1) = check a.z b.z
  in ({dx=x0; dy=y0; dz=z0}, {dx=x1; dy=y1; dz=z1})
;;

let gravity lst =
  List.map (fun a ->
    List.fold_left (fun g b ->
      let (gab, _) = gravity_pair a.p b.p in
      add_gravity g gab
      ) {dx=0; dy=0; dz=0} lst
    ) lst
;;

let gravity_update lst =
  List.map (fun a ->
    let va = List.fold_left (fun g b ->
            let (gab, _) = gravity_pair a.p b.p in
            add_gravity g gab
            ) a.v lst in
    {p=a.p; v=va}
    ) lst
;;

let position_update lst =
  List.map (fun a ->
    let p = a.p and g = a.v in
    {p={x=p.x+g.dx; y=p.y+g.dy; z=p.z + g.dz}; v=a.v}
    ) lst
;;

let energy_unit a =
  let p = a.p and v = a.v in
  let pot = (abs p.x) + (abs p.y) + (abs p.z)
  and kin = (abs v.dx) + (abs v.dy) + (abs v.dz)
  in pot * kin
;;

let energy lst =
  List.fold_left (fun mem a -> mem + energy_unit a) 0 lst
;;

let solve_p1 galaxy k =
  let rec nest ki gala =
  if ki == k then energy gala
  else (
    let p1 = gravity_update gala in
    nest (ki+1) (position_update p1)
    ) in nest 0 galaxy
;;

let check_single a b =
  if ((a.p.x == b.p.x) && (a.p.y = b.p.y) && (a.p.z == b.p.z))
  then ((a.v.dx == b.v.dx) && (a.v.dy == b.v.dy) && (a.v.dz == b.v.dz))
  else false
;;

let rec check_system = function
  | ([], []) -> true
  | (x::lx, y::ly) -> if check_single x y
                      then check_system (lx, ly)
                      else false
  | ([], lx) | (lx, []) -> false
;;

let solve_p2 galaxy =
  let rec nest ki gala =
  if check_system (galaxy, gala) then ki
  else (
    if ki mod 1000000 == 0
    then (
      Printf.printf "step %d\t" ki;
      print_newline ());
    let p1 = gravity_update gala in
    nest (ki+1) (position_update p1)
    ) in nest 1 (position_update (gravity_update galaxy))
;;



let data_test = List.map (scan_input) (String.split_on_char '\n' "<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>");;
let galaxy_test = List.map (fun x -> {p=x; v={dx=0; dy=0; dz=0}}) data_test;;

let data = read_input_data "input_12";;
let galaxy = List.map (fun x -> {p=x; v={dx=0; dy=0; dz=0}}) data;;
