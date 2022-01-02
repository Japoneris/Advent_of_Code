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


type place = Way | Block | Teleporter of int | Visited;;
type position = {x:int; y:int; d:int};;
(*
type teleport = {x:int; y:int; name:int};;
*)
type objet = {p: position; o: place};;

let to_teleport_name a b =
  ((int_of_char a) - 65) * 26 + ((int_of_char b) - 65)
;;

let to_teleport_code name =
  let a = name / 26 +65
  and b = name mod 26 +65 in
  Printf.printf "%c%c" (char_of_int a) (char_of_int b)
;;

let is_char c =
let c0 = int_of_char c in
(65 <= c0) && (c0 <= 90)
;;

let search_teleport mat =
  let ly = Array.length mat
  and lx = Array.length mat.(0) in

  let rec check_horizontal mem i j =
  if i+1 == ly then mem
  else if j+1 == lx then check_horizontal mem (i+1) 1
  else (
    Printf.printf "H: %d %d," i j;
    let c = mat.(i).(j) in
    let c1 = (mat.(i).(j+1)) in
    let c2 = (mat.(i).(j-1)) in

    if  is_char c
    then (
      if (c1 == '.') && (is_char c2)
      then check_horizontal ({x=j+1; y=i; d=to_teleport_name c2 c}:: mem) i (j+1)
      else if (c2 == '.') && (is_char c1)
      then check_horizontal ({x=j-1; y=i; d=to_teleport_name c c1}:: mem) i (j+1)
      else check_horizontal mem i (j+1)
      )
    else check_horizontal mem i (j+1)
  )
  in

  let rec check_vertical mem i j =
  if i+1 == ly then mem
  else if j+1 == lx then check_vertical mem (i+1) 1
  else (
    Printf.printf "V: %d %d," i j;
    let c = mat.(i).(j) in
    let c1 = (mat.(i+1).(j)) in
    let c2 = (mat.(i-1).(j)) in

    if  is_char c
    then (
      if (c1 == '.') && (is_char c2)
      then check_vertical ({x=j; y=i+1; d=to_teleport_name c2 c}:: mem) i (j+1)
      else if (c2 == '.') && (is_char c1)
      then check_vertical ({x=j; y=i-1; d=to_teleport_name c c1}:: mem) i (j+1)
      else check_vertical mem i (j+1)
      )
    else check_vertical mem i (j+1)
  )
  in

  (check_horizontal [] 1 1) @ (check_vertical [] 1 1)
;;



let make_arr_teleport doors =
  let arr = Array.init (26 * 26) (fun _ -> Array.make 2 {x=0; y=0; d=(-1)}) in
  List.iter (fun pt ->
    if arr.(pt.d).(0).d == (-1)
    then arr.(pt.d).(0) <- {x=pt.x; y=pt.y; d=pt.d}
    else arr.(pt.d).(1) <- {x=pt.x; y=pt.y; d=pt.d}
    ) doors;
  arr
;;


let get_best_teleport i j tele =
  Array.fold_left (fun mem row ->
    if (abs (row.(0).x - j) <= 1) && (abs (row.(0).y - i) <= 1)
    then row.(0)
    else if (abs (row.(1).x - j) <= 1) && (abs (row.(1).y - i) <= 1)
    then row.(1)
    else mem
    ) {x=0; y=0; d=0} tele
;;


let get_next_position_d pt =
  List.map (fun (a,b) -> {x=pt.x+a; y=pt.y+b; d=pt.d+1}) [(0, 1); (0, -1); (1, 0); (-1, 0)]
;;

let get_next_position_dd pt =
  List.map (fun (a,b) -> {x=pt.x+a; y=pt.y+b; d=pt.d}) [(0, 1); (0, -1); (1, 0); (-1, 0)]
;;


let go_to_teleport p teleporters =
  let ppp = teleporters.(p.d) in
  if ppp.(0).x == p.x
  then ppp.(1)
  else ppp.(0)
;;






let puzzle = read_input_data "input_20" 135;;
let doors = search_teleport puzzle;;
let teleporters = make_arr_teleport doors;;

search_path puzzle teleporters;;
search_path_v2 puzzle teleporters 50;;


(*899 too high*)
(*675 wrong*)
(*TEST *)

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
               A A D   M                     "
      ;;
let arr_test = Array.of_list (String.split_on_char '\n' data_test);;
let puzzle_test = Array.map (fun line -> let l = String.length line in Array.init l (fun i -> line.[i])) arr_test;;
let doors_test = search_teleport puzzle_test;;
let tele_test = make_arr_teleport doors_test;;
tele_test.(0).(1) <- tele_test.(0).(0);;

search_path puzzle_test tele_test;;
search_path_v2 puzzle_test tele_test 4;;




let search_path puzzle teleporters  =
  let door_end = to_teleport_name 'Z' 'Z' in
  let door_start = to_teleport_name 'A' 'A' in
  let p_end = teleporters.(door_end).(0) in
  let p_start = teleporters.(door_start).(0) in

  let is_finished () =
    puzzle.(p_end.y).(p_end.x) == '#'
  in

  let rec nest d lst =
  if List.length lst == 0 then d
  else if is_finished ()  then d
  else (
    print_puzzle puzzle;

    let valid = List.fold_left (fun mem pt ->
      if is_char puzzle.(pt.y).(pt.x)
      then (
        let pz = get_best_teleport pt.y pt.x teleporters in
        if pz.d == 0
        then mem
        else (
          let px = go_to_teleport pz teleporters in

          if puzzle.(px.y).(px.x) == '.'
          then (
            puzzle.(px.y).(px.x) <- '#';
            px::mem
            ) else mem
        )
      )
      else if puzzle.(pt.y).(pt.x) == '.'
      then (
        puzzle.(pt.y).(pt.x) <- '#';
        pt::mem
        ) else mem
      ) [] lst in

    let next_valid = List.fold_left (fun mem x -> (get_next_position_d x) @ mem) [] valid in

    Printf.printf "Valid: \t";
    List.iter (fun pt -> Printf.printf "(y,x)=(%d, %d)," pt.y pt.x) valid;
    print_newline ();

    print_newline ();
    Printf.printf "Nect valid: ";
    List.iter (fun pt -> Printf.printf "(y,x)=(%d, %d)," pt.y pt.x) next_valid;

    nest (d+1) (next_valid)
    )
  in
  let p0 = p_start in
  nest 0 [p0]
;;



let search_path_v2 puzzle teleporters n =
  (*Make a copy of everything*)
  let levels = Array.init n (fun _ ->
    Array.map (fun row -> Array.copy row) puzzle) in

  let door_end = to_teleport_name 'Z' 'Z' in
  let door_start = to_teleport_name 'A' 'A' in
  let p_end = teleporters.(door_end).(0) in
  let p_start = teleporters.(door_start).(0) in

  Array.iteri (fun i _ -> if i == 0 then ()
    else (
      levels.(i).(p_end.y-1).(p_end.x) <- '#';
      (*
      levels.(i).(p_end.y).(p_end.x+1) <- '#';
      *)
      levels.(i).(p_start.y+1).(p_start.x) <- '#';
      )
  ) levels;

  let lx = Array.length puzzle.(0)
  and ly = Array.length puzzle in


  let is_finished () =
    levels.(0).(p_end.y).(p_end.x) == '#'
  in

  let rec nest d lst =
  if List.length lst == 0 then d
  else if is_finished ()  then d - 1
  else (

    if d mod 1 == 0
    then (
      Printf.printf "Step %d\n" d;
      print_puzzle levels.(0);
      );

    let valid = List.fold_left (fun mem pt ->
      if is_char levels.(pt.d).(pt.y).(pt.x)
      then (
        (*External*)
        if  (pt.x == lx - 2) || (pt.x == 1) || (pt.y = ly-2) || (pt.y = 1)
        then (
          (*Level up*)
            if pt.d == 0
            then mem
            else (
            let pz = get_best_teleport pt.y pt.x teleporters in
            let px = go_to_teleport pz teleporters in
            let py = {x=px.x; y=px.y; d=pt.d-1}  in
            if levels.(py.d).(py.y).(py.x) == '.'
            then (
              if d mod 1 == 0
              then (
              Printf.printf "Teleport from (%d, %d) to (%d, %d) -1, at %d " pt.y pt.x py.y py.x pt.d;
              to_teleport_code px.d;
              print_newline ();
              );

              levels.(py.d).(py.y).(py.x) <- '#';
              levels.(pt.d).(pt.y).(pt.x) <- '#';

              py::mem
              ) else mem
            )
          )
        else (
          if pt.d == n-1 (*Not possible*)
          then mem
          else (
            let pz = get_best_teleport pt.y pt.x teleporters in
            let px = go_to_teleport pz teleporters in
            let py = {x=px.x; y=px.y; d=pt.d+1} in
            if levels.(py.d).(py.y).(py.x) == '.'
            then (
              (*
              Printf.printf "Teleport from (%d, %d) to (%d, %d) +1, at %d " pt.y pt.x py.y py.x pt.d;
              print_newline ();
              *)
              to_teleport_code px.d;
              levels.(pt.d).(pt.y).(pt.x) <- '#';
              levels.(py.d).(py.y).(py.x) <- '#';
              py::mem
              ) else mem
            )
          )
        )
      (*Normal case*)
      else if levels.(pt.d).(pt.y).(pt.x) == '.'
      then (
        levels.(pt.d).(pt.y).(pt.x) <- '#';
        pt::mem ) else mem
      ) [] lst in

    let next_valid = List.fold_left (fun mem x -> (get_next_position_dd x) @ mem) [] valid in

    (*
    Printf.printf "Valide %d | Created %d, " (List.length valid) (List.length next_valid);
    print_newline ();
    *)

    nest (d+1) (next_valid)
    )
  in
  let p0 = p_start in
  nest 0 [p0]
;;


let get_distance puzzle teleport =
  let pzl = Array.map (fun row -> Array.copy row) puzzle in
  let dist = Array.init (26*26) (fun i -> Array.make (26*26) 10000000) in

  let rec nest ori d i j =
  (*
  Printf.printf "%d %d, " i j;
  *)
  if is_char pzl.(i).(j)
  then (
    let pb = get_best_teleport i j teleport in
    dist.(ori).(pb.d) <- d;
    )
  else if pzl.(i).(j) == '#'
  then ()
  else (
    pzl.(i).(j) <- '#';
    List.iter (fun (a,b) -> nest ori (d+1) (i+a) (j+b)) [(0,1); (0,-1); (1, 0); (-1,0)]
    )
   in

  Array.iter (fun y ->
    let x = y.(0) in
    if x.d >= 0
    then (
      Array.iteri (fun i row ->
        pzl.(i) <- Array.copy row
        ) puzzle;
      nest x.d 0 x.y x.x
      );

    let z = y.(1) in
    if z.d >= 0
    then (
      nest z.d 0 z.y z.x
      );
    ) teleport;

  dist
;;

let dtot = get_distance puzzle teleporters;;

Array.iteri (fun i row -> Array.iteri (fun j x ->
  if x < 100000
  then Printf.printf "(%d, %d): \t%d\n" i j x;
  ) row
) dtot;;
  
