let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    nest (( line)::lst)
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    (List.rev lst)
  in nest []
  ;;

let split_input lst =
  let rec nest buff = function
  | [] -> (buff, []) (*Must not appear*)
  | x::lx -> if String.length x < 1 then (List.rev buff, lx)
    else nest (x::buff) lx
    in nest [] lst
;;


let convert_line stri =
  let l = String.length stri in
  Array.init l (fun i -> stri.[i] == '#')
;;

let convert_input lst =
  let (p0, p1) = split_input lst in
  (Array.concat  (List.map (convert_line) p0), (Array.map (convert_line) (Array.of_list p1)))
;;



let extend_board img =
  let l0 = Array.length img
  and l1 = Array.length img.(0) in

  Array.init (l0+4) (fun i ->
    if (i <= 1) || (i > l0+1)
    then Array.make (l1+4) false
    else
    Array.init (l1+4) (fun j -> (*max: l1+1 *)
      if (j <= 1) || (j > l1+1)
      then false
      else img.(i-2).(j-2)
      )
    )
;;


let extend_board_infinite img k =
  let l0 = Array.length img
  and l1 = Array.length img.(0) in

  Array.init (l0+k*2) (fun i ->
    if (i <= k-1) || (i > l0+k-1)
    then Array.make (l1+k*2) false
    else
    Array.init (l1+2*k) (fun j -> (*max: l1+1 *)
      if (j <= k-1) || (j > l1+k-1)
      then false
      else img.(i-k).(j-k)
      )
    )
;;


let print_board mat =
  Array.iter (fun row ->
    Array.iter (fun x ->
      Printf.printf  (if x then "#" else ".")) row;
      print_newline ()
    ) mat
;;


let to_bin lst =
  let rec nest mem = function
  | [] -> mem
  | x::lx -> if x
    then nest (2*mem+1) lx
    else nest (2*mem) lx
  in nest 0 lst
;;

let update_mat rules mat =
  let l0 = Array.length mat
  and l1 = Array.length mat.(0) in

  let get_value i j =
    let vi = to_bin [
      mat.(i-1).(j-1); mat.(i-1).(j); mat.(i-1).(j+1);
      mat.(i).(j-1);   mat.(i).(j);   mat.(i).(j+1);
      mat.(i+1).(j-1); mat.(i+1).(j); mat.(i+1).(j+1)]
      in
      rules.(vi)
  in
  Array.init (l0-2) (fun i ->
    Array.init (l1-2) (fun j ->
      get_value (i+1) (j+1)
      )
    )
;;
(*
  Array.init l0 (fun i ->
    if (i == 0) || (i == l0-1)
    then Array.make l1 false
    else
    Array.init l1 (fun j ->
      if (j == 0) || (j == l1-1)
      then false
      else get_value i j
      )
    )
    *)
;;




let update_puzzle rules mat k =

  let rec nest m1 ki =
  if ki == k then m1
  else (
    let m3 = update_mat rules m1 in
    nest m3 (ki+1)
    ) in
  nest (extend_board_infinite mat (k*2+1)) 0
;;

let count_pixel mat =
  Array.fold_left (fun mem row ->
    Array.fold_left (fun m0 x -> if x then m0 + 1 else m0) mem row
    ) 0 mat
;;

let lst = read_file "input_20_test";;
let lst = read_file "input_20.txt";;
let (v0, img) = convert_input lst;;


let m1 = update_puzzle v0 img 2;;
print_board m1;;
count_pixel m1;;

let m2 = update_puzzle v0 img 50;;
print_board m2;;
count_pixel m2;;


(*6128 too high*)
