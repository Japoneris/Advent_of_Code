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

let data = read_file "input_17";;
let puzzle = transform data;;

let data_t = read_file "input_17_test";;
let puzzle_t = transform data_t;;


type place =
| Empty
| Full
;;

let transform_line line =
  let l = String.length line in
  Array.init l (fun i ->
    if line.[i] == '.'
    then Empty
    else Full
    );;

let transform input =
  Array.of_list (List.map (transform_line) input)
  ;;


let empty_frame l0 l1 =
  Array.init l0 (fun i ->
    Array.make l1 Empty
    );;

let check_neighbors bigpuzzle i j k =
  let l0 = Array.length bigpuzzle
  and l1 = Array.length bigpuzzle.(0)
  and l2 = Array.length bigpuzzle.(0).(0)
  in

  let rec nest cnt i0 j0 k0 =
  if (k0 - k >= 2) || (k0 >= l2)
  then cnt
  else if (j0 - j >= 2) || (j0 >= l1)
  then nest cnt (i-1) (j-1) (k0+1)
  else if (i0 - i >= 2) || (i0 >= l0)
  then nest cnt (i-1) (j0+1) k0
  else if i0 < 0
  then nest cnt (i0+1) j0 k0
  else if j0 < 0
  then nest cnt i0 (j0+1) k0
  else if k0 < 0
  then nest cnt i0 j0 (k0+1)
  else match  bigpuzzle.(i0).(j0).(k0) with
    | Empty -> nest cnt (i0+1) j0 k0
    | Full -> nest (cnt+1) (i0+1) j0 k0
  in nest 0 (i-1) (j-1) (k-1)
;;


let update_map bigpuzzle =
  let l0 = Array.length bigpuzzle
  and l1 = Array.length bigpuzzle.(0)
  and l2 = Array.length bigpuzzle.(0).(0)
  in
  Array.init l0 (fun i ->
    Array.init l1 (fun j ->
      Array.init l2 (fun k ->
        let c = check_neighbors bigpuzzle i j k in
        match bigpuzzle.(i).(j).(k) with
        | Empty -> if c > 3
          then Full
          else Empty
        | Full -> if (c == 3) || (c=4)
          then Full
          else Empty
        )
      )
    )
  ;;

let count_active bigpuzzle =
  Array.fold_left (fun mem a0 ->
    mem + Array.fold_left (fun m1 a1 ->
      m1 + Array.fold_left (fun m2 v2 ->
        match v2 with
        | Full -> 1 + m2
        | Empty -> 0 + m2

        ) 0 a1
      ) 0 a0
    ) 0 bigpuzzle
;;

let increase_map puzzle n =
  let l0 = Array.length puzzle
  and l1 = Array.length puzzle.(0)
  in
  Array.init (1 + 2*n) (fun i ->
    Array.init (l1 + 2*n) (fun j ->
      Array.init (l0 + 2*n) (fun k ->
          if ((i == n) &&
            (j >= n) && (j < n + l0) &&
            (k >= n) && (k < n + l1)
            )
          then puzzle.(j-n).(k-n)
          else Empty
        )
      )
    )
;;


let print_puzzl big_puzzle =
  Array.iter (fun mem ->
    print_newline ();
    print_newline ();
    Array.iter (fun row ->
      Array.iter (fun i ->
        match  i with
        | Full -> Printf.printf "#"
        | Empty -> Printf.printf "."
        ) row;
      print_newline ()
      ) mem
    ) big_puzzle;;


let solve_1 puzzle n =

  let big_puzzle = increase_map puzzle n in


  let rec nest i big =
  if i == n
  then count_active big
  else nest (i+1) (update_map big)
  in nest 0 big_puzzle

;;



let increase_map_2 puzzle n =
  let l0 = Array.length puzzle
  and l1 = Array.length puzzle.(0)
  in
  Array.init (1 + 2*n) (fun m ->
    Array.init (1 + 2*n) (fun i ->
      Array.init (l1 + 2*n) (fun j ->
        Array.init (l0 + 2*n) (fun k ->
            if ((i == n) && (m == n) &&
              (j >= n) && (j < n + l0) &&
              (k >= n) && (k < n + l1)
              )
            then puzzle.(j-n).(k-n)
            else Empty
          )
        )
      )
    )
;;


let count_active_2 bigpuzzle =
Array.fold_left (fun m a ->
  m + Array.fold_left (fun mem a0 ->
    mem + Array.fold_left (fun m1 a1 ->
      m1 + Array.fold_left (fun m2 v2 ->
        match v2 with
        | Full -> 1 + m2
        | Empty -> 0 + m2

        ) 0 a1
      ) 0 a0
    ) 0 a
  ) 0 bigpuzzle
;;


let check_neighbors_2 bigpuzzle i j k n =
  let l0 = Array.length bigpuzzle
  and l1 = Array.length bigpuzzle.(0)
  and l2 = Array.length bigpuzzle.(0).(0)
  and l3 = Array.length bigpuzzle.(0).(0).(0)
  in

  let rec nest cnt i0 j0 k0 n0 =
  if (n0 - n >= 2) || (n0 >= l3)
  then cnt (*Terminal*)
  else if (k0 - k >= 2) || (k0 >= l2)
  then nest cnt (i-1) (j-1) (k-1) (n0+1)
  else if (j0 - j >= 2) || (j0 >= l1)
  then nest cnt (i-1) (j-1) (k0+1) n0
  else if (i0 - i >= 2) || (i0 >= l0)
  then nest cnt (i-1) (j0+1) k0 n0
  else if i0 < 0
  then nest cnt (i0+1) j0 k0 n0
  else if j0 < 0
  then nest cnt i0 (j0+1) k0 n0
  else if k0 < 0
  then nest cnt i0 j0 (k0+1) n0
  else if n0 < 0
  then nest cnt i0 j0 k0 (n0+1)
  else match  bigpuzzle.(i0).(j0).(k0).(n0) with
    | Empty -> nest cnt (i0+1) j0 k0 n0
    | Full  -> nest (cnt+1) (i0+1) j0 k0 n0
  in nest 0 (i-1) (j-1) (k-1) (n-1)
;;



let update_map_2 bigpuzzle =
  let l0 = Array.length bigpuzzle
  and l1 = Array.length bigpuzzle.(0)
  and l2 = Array.length bigpuzzle.(0).(0)
  and l3 = Array.length bigpuzzle.(0).(0).(0)
  in

  Array.init l0 (fun i ->
    Array.init l1 (fun j ->
      Array.init l2 (fun k ->
        Array.init l3 (fun l ->
          let c = check_neighbors_2 bigpuzzle i j k l in
          match bigpuzzle.(i).(j).(k).(l) with
          | Empty -> if c == 3
            then Full
            else Empty
          | Full -> if (c == 3) || (c=4)
            then Full
            else Empty
          )
        )
      )
    )
  ;;


let solve_2 puzzle n =

  let big_puzzle = increase_map_2 puzzle n in


  let rec nest i big =
  if i == n
  then count_active_2  big
  else nest (i+1) (update_map_2 big)
  in nest 0 big_puzzle

;;
