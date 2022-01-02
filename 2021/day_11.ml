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

let grid = read_file "input_11.txt";;

let update grid =
  let l0 = Array.length grid
  and l1 = Array.length grid.(0) in

  (*If false, not flashed yet*)
  let mat_bool = Array.mapi (fun i row ->
    Array.mapi (fun j x ->
        grid.(i).(j) <- grid.(i).(j) + 1;
        false
      ) row
    ) grid in

  let flash i j =
    if (i < 0) || (j < 0) || (i >= l0) || (j >= l1) then ()
    else if mat_bool.(i).(j) then ()
    else grid.(i).(j) <- grid.(i).(j) + 1
  in

  let rec nest i j =
  if (i < 0) || (j < 0) || (i >= l0) || (j >= l1) then ()
  else if mat_bool.(i).(j)
  then ()
  else if (grid.(i).(j) >= 10) && (mat_bool.(i).(j) == false)
  then (*Flash*) (
    mat_bool.(i).(j) <- true;
    grid.(i).(j) <- 0;
    flash i (j+1); flash i (j - 1); flash (i+1) j; flash (i-1) j;
    flash (i+1) (j+1); flash (i+1) (j-1); flash (i-1) (j+1); flash (i-1) (j-1);
    nest i (j+1); nest i (j - 1); nest (i+1) j; nest (i-1) j;
    nest (i+1) (j+1); nest (i+1) (j-1); nest (i-1) (j+1); nest (i-1) (j-1);
    )
  else ()
  in
  (*Activation phase*)
  Array.iteri (fun i row ->
    Array.iteri (fun j x -> nest i j
      ) row
    ) grid;

  (*Count*)
  Array.fold_left (fun mem row ->
    Array.fold_left (fun m1 tf -> if tf then m1 + 1 else m1) mem row
    ) 0 mat_bool
;;

let show_mat grid =
  Array.iter (fun row ->
    Array.iter (fun x -> Printf.printf "%d" x) row;
    print_newline()
    ) grid
;;

let grid = read_file "input_11_test";;
let grid = read_file "input_11.txt";;

let arr = Array.init 100 (fun _ -> update grid);;
Array.fold_left (+) 0 arr;;


let check_best_level grid =
  let l0 = Array.length grid
  and l1 = Array.length grid.(0) in
  let maxi = l0 * l1 in

  let rec nest i =
    let flx = update grid in
    Printf.printf "Step %d: %d flashes" i flx;
    print_newline ();
    if flx == maxi
    then i
    else nest (i+1)
  in nest 1
;;
