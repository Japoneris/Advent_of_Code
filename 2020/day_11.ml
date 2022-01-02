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

type place =
| Occup
| Empty
| Floor
;;

let data = read_file "input_11";;

let transform_line line =
  let l1 = String.length line in
  Array.init l1 (fun i ->
    if line.[i] == 'L'
    then Empty
    else Floor
    );;

let seatmap = Array.of_list (List.map (transform_line) data);;

let check_seat i j seats =
  let li = Array.length seats
  and lj = Array.length seats.(0)
  in

  let rec nest cnt ci cj =
    if cj == 2
    then cnt
    else if ci == 2
    then nest cnt (-1) (cj+1)
    else if ((ci == 0) && (cj==0)) || ((i + ci) >= li) || ((j+cj) >= lj) || (i+ci < 0) || (j+cj < 0)
    then nest cnt (ci+1) cj
    else match seats.(i+ci).(j+cj) with
    | Empty | Floor -> nest cnt (ci+1) cj
    | Occup -> nest (cnt+1) (ci+1) cj

  in nest 0 (-1) (-1)
;;

let update_seatmap seats =
  let li = Array.length seats
  and lj = Array.length seats.(0)
  in

  Array.init li (fun i ->
    Array.init lj (fun j ->
      match seats.(i).(j) with
      | Floor -> Floor
      | Occup -> (
        let cnt = check_seat i j seats in
        if cnt >= 4
        then Empty
        else Occup
        )
      | Empty -> (
        let cnt = check_seat i j seats in
        if cnt == 0
        then Occup
        else Empty
        )
      )
    )
  ;;

let count_seats seats =
  let li = Array.length seats
  and lj = Array.length seats.(0)
  in
  let rec nest cnt i j =
  if i >= li
  then nest cnt 0 (j+1)
  else if j >= lj
  then cnt (*Exit*)
  else match seats.(i).(j) with
  | Occup -> nest (cnt+1) (i+1) j
  | _ ->  nest cnt (i+1) j
  in nest 0 0 0
;;

let solve_1 seats =
  let rec nest ni seats_i =
    Printf.printf "%d" ni;
    print_newline();
  let seats_j = update_seatmap seats_i in
  let nj = count_seats seats_j in
  if ni == nj
  then nj
  else nest nj seats_j
  in nest (-1) seats
;;


let beam_seat di dj i j seats =
  let li = Array.length seats
  and lj = Array.length seats.(0) in
  let rec nest i0 j0 =
  if (i0 >= li) || (j0 >= lj) || (i0 < 0) || (j0 < 0)
  then 0
  else match seats.(i0).(j0) with
  | Occup -> 1
  | Empty -> 0
  | Floor -> nest (i0+di) (j0+dj)
  in nest (i+di) (j+dj)
;;


let check_seat_2 i j seats =
  let rec nest cnt ci cj =
  if cj == 2
  then cnt
  else if ci == 2
  then nest cnt (-1) (cj+1)
  else if (ci == 0) && (cj == 0)
  then nest cnt (ci+1) cj
  else nest (cnt + (beam_seat ci cj i j seats)) (ci+1) cj
  in nest 0 (-1) (-1)
;;

let update_seatmap_2 seats =
  let li = Array.length seats
  and lj = Array.length seats.(0)
  in

  Array.init li (fun i ->
    Array.init lj (fun j ->
      match seats.(i).(j) with
      | Floor -> Floor
      | Occup -> (
        let cnt = check_seat_2 i j seats in
        if cnt >= 5
        then Empty
        else Occup
        )
      | Empty -> (
        let cnt = check_seat_2 i j seats in
        if cnt == 0
        then Occup
        else Empty
        )
      )
    )
  ;;


  let solve_2 seats =
    let rec nest ni seats_i =
      Printf.printf "%d" ni;
      print_newline();
    let seats_j = update_seatmap_2 seats_i in
    let nj = count_seats seats_j in
    if ni == nj
    then nj
    else nest nj seats_j
    in nest (-1) seats
  ;;
