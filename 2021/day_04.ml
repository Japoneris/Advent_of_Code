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


let convert_line line =
  Array.of_list (List.init 5 (fun i ->
    let s0 = String.sub line (i*3) 2 in
    if s0.[0] == ' '
    then int_of_string (String.sub s0 1 1)
    else int_of_string s0
    ))
  ;;

let convert_board board =
  let l1 = List.map (fun stri ->
    convert_line stri
    ) board in
  Array.of_list l1
;;

let decompose_puzzle lst =
  let l1::lx = lst in
  let rec nest boards buff = function
  | [] -> if List.length buff == 0
    then boards
    else buff::boards
  | line::lz -> if String.length line < 4
    then if List.length buff == 0
      then nest boards buff lz
      else nest (buff::boards) [] lz
    else nest boards (line::buff) lz
  in
  let boards = nest [] [] lx  in
  let l2 = List.map (fun x -> int_of_string x) (String.split_on_char ',' l1) in
  let l3 = List.map (convert_board) boards in
  (l2, l3)

;;



let check_bingo board_bool =
  let rec nest i =
  if i == 5
  then false
  else (
    let v1 = Array.fold_left (&&) true board_bool.(i) in (*Horizontal*)
    let (v2, _) = Array.fold_left (fun (mem, j) _ -> (mem && board_bool.(j).(i), j+1 )) (true, 0) board_bool in
    if v1 || v2 then true
    else nest (i+1)
    )
  in nest 0
;;

let create_bool_board x =
  Array.init x (fun _ ->
    Array.init x (fun _ -> false
      )
    )
;;

let turn_on board board_bool v =
  Array.iteri (fun i row ->
    Array.iteri (fun j x ->
      if x == v then board_bool.(i).(j) <- true
      ) row
    ) board
;;



let count_board_solution board board_bool =
  let a1 = Array.mapi (fun i row ->
    Array.mapi (fun j x ->
      if x then 0
      else board.(i).(j)
      ) row
    ) board_bool in

  Array.fold_left (fun mem row -> mem + (Array.fold_left (+) 0 row)) 0 a1
;;

let check_the_bingo bingo_nbr boards =
  let pairs = List.map (fun x -> (x, create_bool_board 5)) boards in

  let rec nest = function
  | [] -> ()
  | nbr::lst -> (
    Printf.printf "Round %d\n" nbr;
    List.iter (fun (x, y) -> turn_on x y nbr) pairs;
    let l1 = List.mapi (fun i (x, y) -> (i, check_bingo y)) pairs in
    let l2 = List.fold_left (fun mem (a, b) -> if b then a::mem else mem) [] l1 in

    let d1 = (List.length l2)  in
    if d1 >= 1
    then (
      Printf.printf "%d Winning boards\n" d1;
      List.iter (fun i ->

        let (p1, p2) = List.nth pairs i in
        let r =  count_board_solution p1 p2 in
        Printf.printf "%d => %d\n" r (r * nbr);
        ) l2
      )
    else nest lst
    ) in
    nest bingo_nbr
;;



let check_the_bingo_last bingo_nbr boards =
  let pairs = List.map (fun x -> (x, create_bool_board 5)) boards in

  let rec nest left_pairs = function
  | [] -> ()
  | nbr::lst -> (
    Printf.printf "Round %d\n" nbr;
    List.iter (fun (x, y) -> turn_on x y nbr) left_pairs;
    let left_new = List.fold_left (fun mem (x, y) -> if check_bingo y then mem else (x,y)::mem) [] left_pairs in

    let d1 = (List.length left_pairs)
    and d2 = (List.length left_new)  in
    Printf.printf "%d / %d Losing boards\n" d2 d1;

    if (d1 == 1) && (d2 == 0)
    then (
      List.iter (fun (p1, p2) ->
        let r =  count_board_solution p1 p2 in
        Printf.printf "%d => %d\n" r (r * nbr);
        ) left_pairs
      )
    else nest left_new lst
    ) in
    nest pairs bingo_nbr
;;



let lst = read_file "input_04.txt";;
let (bingo, boards) = decompose_puzzle lst;;
check_the_bingo bingo boards;;
