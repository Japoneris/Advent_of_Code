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

let data = read_file "input_10";;
let numbers = List.map (fun line ->
     Scanf.sscanf line "%d " (fun x -> x)
     ) data;;

let adaptators = List.sort (-) numbers;;

let solve_1 adapt =
  let rec nest n1 n2 n3 vi = function
  | [] -> (n1, n2, n3, vi)
  | x::lx -> if x - vi == 1
    then nest (n1+1) n2 n3 x lx
    else if x - vi == 2
    then nest n1 (n2+1) n3 x lx
    else if x - vi == 3
    then nest n1 n2 (n3+1) x lx
    else (Printf.printf "Error: %d=> %d\n" vi x;
    nest n1 n2 n3 x lx
    )
    in nest 0 0 0 0 adapt
;;

  let (a1, _, a3, _) = solve_1 adaptators in
  Printf.printf "%d * (%d + 1) = %d\n" a1 a3 (a1*(a3+1));;


(*lst: (value, count)*)
let matching_count v0 lst =
  List.fold_left (fun mem (vi, ci) ->
    if v0 - vi == 1
    then mem + ci
    else if v0 - vi == 2
    then mem + ci
    else if v0-vi == 3
    then mem + ci
    else mem
  ) 0 lst
;;

let solve_2 adapt =
  let rec nest lst = function
  | [] -> List.hd lst
  | x::lx ->
    let c0 = matching_count x lst in
    nest ((x, c0)::lst) lx
  in nest [(0, 1)] adapt
;;
