let parse_line line =
  let l = String.length line in
  Array.init l (fun i -> int_of_char (line.[i]) - 48)
  ;;

let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    nest ((parse_line line)::lst)
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    Array.of_list (List.rev lst)
  in nest []
  ;;

let map = read_file "input_09.txt";;

let check_location map =
  let l0 = Array.length map
  and l1 = Array.length map.(0)
  in

  (*border*)
  let b0 = Array.init (l0-2) (fun i ->
    let j = i + 1 in
    let v0 = map.(j).(0) in
    if (v0 < map.(j).(1)) && (v0 < map.(j+1).(0)) && (v0 < map.(j-1).(0))
    then 1+v0 else 0
    )

  and b1 = Array.init (l0-2) (fun i ->
    let j = i + 1 in
    let v0 = map.(j).(l1-1) in
    if (v0 < map.(j).(l1-2)) && (v0 < map.(j+1).(l1-1)) && (v0 < map.(j-1).(l1-1))
    then 1+v0 else 0
    )

  and b2 = Array.init (l1 - 2) (fun i ->
    let j = i + 1 in
    let v0 = map.(0).(j) in
    if (v0 < map.(1).(j)) && (v0 < map.(0).(j+1)) && (v0 < map.(0).(j-1))
    then 1+v0 else 0
    )

  and b3 = Array.init (l1 - 2) (fun i ->
    let j = i + 1 in
    let v0 = map.(l0-1).(j) in
    if (v0 < map.(l0-2).(j)) && (v0 < map.(l0-1).(j+1)) && (v0 < map.(l0-1).(j-1))
    then 1+v0 else 0
    )

  and c0 = if (map.(0).(0) < map.(1).(0)) && (map.(0).(0) < map.(0).(1)) then 1 + map.(0).(0) else 0
  and c1 = if (map.(l0-1).(0) < map.(l0-2).(0)) && (map.(l0-1).(0) < map.(l0-1).(1)) then 1 + map.(l0-1).(0) else 0
  and c2 = if (map.(l0-1).(l1-1) < map.(l0-2).(l1-1)) && (map.(l0-1).(l1-1) < map.(l0-1).(l1-2)) then 1 + map.(l0-1).(l1-1) else 0
  and c3 = if (map.(0).(l1-1) < map.(1).(l1-1)) && (map.(0).(l1-1) < map.(0).(l1-2)) then 1 + map.(0).(l1-1) else 0
  in

  let btot = (Array.fold_left (+) 0 b0) + (Array.fold_left (+) 0 b1) + (Array.fold_left (+) 0 b2) + (Array.fold_left (+) 0 b3)
  and ctot = c0 + c1 + c2 + c3
  in

  let map2 = Array.mapi (fun i row ->
    if (i == 0) || (i==l0-1)
    then 0
    else
      let arr2 = Array.mapi (fun j x ->
        if (j==0) || (j == l1-1) then 0
        else
        let v0 = map.(i).(j) in
        if (v0 < map.(i).(j+1)) && (v0 < map.(i).(j-1)) && (v0 < map.(i+1).(j)) && (v0 < map.(i-1).(j))
        then v0 + 1 else 0
        ) row in
      Array.fold_left (+) 0 arr2
     ) map in
  Array.fold_left (+) (ctot + btot) map2
;;

let res = check_location map;;
Printf.printf "Total cost: %d\n" res;;

(*Part 2
Check connected components limited by a 9
*)

let check_bassin map =
  let l0 = Array.length map
  and l1 = Array.length map.(0) in

  let mapbool = Array.map (fun row ->
    Array.map (fun x -> x == 9) row
    ) map in

  let rec nest i j =
    if (i == l0) || (i < 0) || (j == l1) || (j < 0) then 0
    else if mapbool.(i).(j)
    then 0
    else (
      mapbool.(i).(j) <- true;
      1  + (nest (i+1) j) + (nest i (j+1)) + (nest i (j-1)) + (nest (i-1) j)
      )
  in

  let rec check i j mem =
  if i == l0
  then mem (*End criterion*)
  else if j == l1
  then check (i+1) 0 mem
  else (
    let a = nest i j in
    if a == 0
    then check i (j+1) mem
    else check i (j+1) (a::mem)
    )
  in
  let lst = check 0 0 [] in
  let (x::y::z::leftover) = List.rev (List.sort (-) lst) in
  Printf.printf "%d x %d x %d = %d\n" x y z (x*y*z)
;;

(*53954872- too high
371448 too low*)
