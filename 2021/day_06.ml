let parse_line line =
  let lst = String.split_on_char ',' line in
  List.map (int_of_string) lst
;;

let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    parse_line line
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    (List.rev lst)
  in nest []
  ;;


let lst = read_file "input_06.txt";;

let brute_force days c0 =
  let rec nest di cnt =
  if di == days
  then (
      Printf.printf  "\tEnd with a fish with cnt = %d\n" cnt;
      1
    )
  else if cnt == 0
  then (
    Printf.printf "Create a fish at day %d\n" di;
    (nest (di+1) 6) + (nest (di+1) 8)
    )
  else nest (di+1) (cnt-1)
  in nest 0 c0
;;

let arr = Array.init 6 (fun i -> brute_force 80 i);;

let res = List.fold_left (fun mem i -> mem + arr.(i)) 0 lst;;

Printf.printf "Number of lanternfish: %d\n" res;;


(*Too long*)
let brute_force days c0 =
  let rec nest di cnt =
  if di == days
  then 1
  else if cnt == 0
  then (
    (nest (di+1) 6) + (nest (di+1) 8)
    )
  else nest (di+1) (cnt-1)
  in nest 0 c0
;;



let recursive_count days =
  (*j: time elapsed*)
  let mat = Array.init 9 (fun _ -> Array.make (days+1) 0) in
  let _ = Array.init 9 (fun i -> mat.(i).(0) <- 1) in

  (*ci: compteur courant / di: temps de vie*)
  let rec nest di ci =
    if mat.(ci).(di) > 0
    then mat.(ci).(di) (*If value already computed*)
    else if ci == 0
    then (
      let a = nest (di-1) 6
      and b = nest (di-1) 8 in
      mat.(ci).(di) <- a + b;
      a + b
      )
    else (
      mat.(ci).(di) <- nest (di-1) (ci-1);
      mat.(ci).(di)
      )
  in
  let _ = Array.init 9 (fun i -> nest days i) in
  Array.init 9 (fun i -> mat.(i).(days))
;;

let arr =  recursive_count 256;;
let res = List.fold_left (fun mem i -> mem + arr.(i)) 0 lst;;

Printf.printf "Number of lanternfish: %d\n" res;;
