let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    nest ((to_chars line)::lst)
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    Array.of_list (List.rev lst)
  in nest []
  ;;

  let to_chars stri =
    let l = String.length stri in
    Array.init l (fun i -> stri.[i])
  ;;


let check_move_h mat =
  let l0 = Array.length mat
  and l1 = Array.length mat.(0) in

  Array.init l0 (fun i ->
    Array.init l1 (fun j ->
      if mat.(i).(j) == '>'
      then mat.(i).((j+1) mod l1) == '.'
      else false
      )
    )
;;

let check_move_v mat =
  let l0 = Array.length mat
  and l1 = Array.length mat.(0) in

  Array.init l0 (fun i ->
    Array.init l1 (fun j ->
      if mat.(i).(j) == 'v'
      then mat.((i+1) mod l0).(j) == '.'
      else false
      )
    )
;;

let update_position mat tf_mat =
  let l0 = Array.length mat
  and l1 = Array.length mat.(0) in

  let mat_new = Array.init l0 (fun _-> Array.make l1 '.') in

  Array.iteri (fun i row ->
    Array.iteri (fun j tf ->
      if tf then (
        if mat.(i).(j) == 'v'
        then mat_new.((i+1) mod l0).(j) <- 'v'
        else mat_new.(i).((j+1) mod l1) <- '>';
      )
      else if mat.(i).(j) != '.'
      then mat_new.(i).(j) <- mat.(i).(j)
      ) row
    ) tf_mat;
  mat_new
;;




let count_moves mat =
  Array.fold_left (fun m0 row ->
    Array.fold_left (fun m1 x -> if x then m1+1 else m1) m0 row
    ) 0 mat
;;

let count_items mat =
  Array.fold_left (fun m0 row ->
    Array.fold_left (fun m1 x -> if x == '.' then m1 else m1+1) m0 row
    ) 0 mat
;;


let print_mat mat =
  Array.iter (fun row ->
    Array.iter (Printf.printf "%c") row;
    print_newline ()
    ) mat
  ;;


let update_single_step mat =
  let mat_tf1 = check_move_h mat in
  let c1 = count_moves mat_tf1 in
  let mat1 = update_position mat mat_tf1 in

  let mat_tf2 = check_move_v mat1  in
  let c2 = count_moves mat_tf2 in
  let mat2 = update_position mat1 mat_tf2 in


  Printf.printf "%d + %d positions updatable: %d" c1 c2 (c1+c2);
  print_newline ();
  (mat2, c1+c2)
;;

let solve_part_1 puzzle =
  let rec nest i mat =
    let (mat1, cc) = update_single_step mat in
    if cc == 0
    then (i, mat1)
    else nest (i+1) mat1
  in nest 1 puzzle
  ;;

let mat0 = read_file "input_25.txt";;
let mat0 = read_file "input_25_test";;
solve_part_1 mat0;;
