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

let data = read_file "input_22";;
let data_t = read_file "input_22_t";;

let extract_player data =
  let rec nest mem = function
  | [] -> (List.rev mem, [])
  | x::lx -> if String.length x == 0
    then (List.rev mem, lx)
    else if x.[0] == 'P'
    then nest [] lx
    else nest ((int_of_string x)::mem) lx
  in nest [] data
;;



let get_score lst =
  let rec nest mem i = function
  | [] -> mem
  | x::lx -> nest ((i*x)+mem) (i+1) lx
  in nest 0 1 (List.rev lst)
;;


let play_game_1 play_1 play_2 =
  let rec nest = function
  | ([], x) -> (
    let v = get_score x in
    Printf.printf "Player 2 win ! with S=%d\n" v;
    )
  | (x, []) -> (
    let v = get_score x in
    Printf.printf "Player 1 win ! with S=%d\n" v;
    )
  | (x::lx, y::ly) -> (

  Printf.printf "%d vs %d (%d %d)\n" x y (List.length lx) (List.length ly);
  if x > y
  then nest (lx@[x; y], ly)
  else nest (lx, ly @ [y; x])
  )
  in nest (play_1, play_2)
;;

let solve_1 data =
  let (play_1, left) = extract_player data in
  let (play_2, _) = extract_player left in

  play_game_1 play_1 play_2
;;

(*Part 2 *)

let list_sub lst k =
  let rec nest mem i = function
  | [] -> List.rev mem
  | x::lx ->
    if i == k
    then List.rev mem
    else nest (x::mem) (i+1) lx
  in nest [] 0 lst
;;

let check_value lst v0 v1 =
  List.fold_left (fun mem (x0, x1) ->
  mem || ((x0==v0) && (x1==v1))) false lst
;;

let rec play_game_2 play_1 play_2 =
  let rec nest prev_deck = function
  | ([], x) -> (
    Printf.printf "Player 2 win !\n";
    (false, x)
    )
  | (x, []) -> (
    Printf.printf "Player 1 win !\n";
    (true, x)
    )
  | (x::lx, y::ly) -> (
    let vx = get_score lx
    and vy = get_score ly in
    let tf = check_value prev_deck vx vy in
    if tf
    then (
      Printf.printf "Player 1 won by default";
      (true, [])
      )
    else (
    let l0 = List.length lx
    and l1 = List.length ly in
    if (x <= l0) && (y <= l1)
    then (
      Printf.printf "==== Play subgame (%d %d) ==== \n" x y;
      let (tf, results) = play_game_2  (list_sub lx x) (list_sub ly y) in
      if tf
      then nest ((vx, vy)::prev_deck) (lx@[x; y], ly)
      else nest ((vx, vy)::prev_deck) (lx, ly @ [y; x])
      )
    else (
      Printf.printf "%d vs %d (%d %d)\n" x y (List.length lx) (List.length ly);
      if x > y
      then nest ((vx, vy)::prev_deck) (lx@[x; y], ly)
      else nest ((vx, vy)::prev_deck) (lx, ly @ [y; x])
      )
    )
  )
  in nest [] (play_1, play_2)
;;


let solve_2 data =
  let (play_1, left) = extract_player data in
  let (play_2, _) = extract_player left in
  let (tf, deck) = play_game_2 play_1 play_2 in
  tf, deck, get_score deck
;;
