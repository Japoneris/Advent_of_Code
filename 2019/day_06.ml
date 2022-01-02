let c_to_val x =
  let v0 = int_of_char x in
  if v0 > 60
  then v0 - 55
  else v0 - 48
;;

let string_to_code x =
  (c_to_val x.[0]) + 35 * (c_to_val x.[1] + 35 * (c_to_val x.[2]))
;;

let process x =
  let (a::b::[]) = String.split_on_char ')' x in
  (string_to_code a, string_to_code b);;

let read_orbits path =
    let ic = open_in path in

    let rec nest lst =
    try
      nest ((process (input_line ic))::lst)
     with e ->
     close_in_noerr ic;
     lst in
    nest []
  ;;


let data = read_orbits "input_06";;
let data_test = read_orbits "input_06_test";;

let solve_1 graph =
  (*Parent of*)
  (*Children of*)
  let parent = Array.make (35*35*35*35) []
  and childr = Array.make (35*35*35*35) (-1)
  in

  List.iter (fun (a, b) ->
    parent.(a) <- b::parent.(a);
    childr.(b) <- a
  ) graph;


  let (root, _) = Array.fold_left (fun (mem, c) v ->
    if (v == (-1)) && (List.length parent.(c) > 0)
    then (c, c+1) else (mem, c+1)
   ) (-1, 0) childr
  in

  let counter = Array.make (35*35*35*35) 0 in
  let rec nest pi ci =
    counter.(pi) <- ci;
    List.iter (fun px -> nest px (ci+1)) parent.(pi)
    in
  nest root 0;
  Array.fold_left (+) 0 counter
;;

let result_1 = solve data;;

let solve_2 graph =
  (*Parent of*)
  (*Children of*)
  let parent = Array.make (35*35*35*35) []
  and childr = Array.make (35*35*35*35) (-1)
  in
  List.iter (fun (a, b) ->
    parent.(a) <- b::parent.(a);
    childr.(b) <- a
  ) graph;

  (*Sequence to go to root*)
  let rec nest lst x =
  if childr.(x) == -1 then lst
  else nest (x::lst) childr.(x)
  in

  let you   = nest [] (string_to_code "YOU")
  and santa = nest [] (string_to_code "SAN")
  in

  let rec matching = function
  | ([], []) -> 0
  | (x, []) | ([], x) -> List.length x
  | (x::lx, y::ly) -> if x == y then matching (lx, ly)
    else (List.length lx) + (List.length ly)
  in
  matching (you, santa)
;;
