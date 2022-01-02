let parse_line line =
  let (a::b::[]) = String.split_on_char '|' line in
  let lst_a = String.split_on_char ' ' (String.sub a 0 ((String.length a) - 1))
  and lst_b = String.split_on_char ' ' (String.sub b 1 ((String.length b) - 1)) in
  (lst_a, lst_b)
;;

let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    nest ((parse_line line)::lst)
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    (List.rev lst)
  in nest []
  ;;


let part_1_easy xb =
  List.fold_left (fun mem word ->
    let l = String.length word in
    if l == 2 then mem + 1
    else if l == 4 then mem + 1
    else if l == 3 then mem + 1
    else if l == 7 then mem + 1
    else mem
    ) 0 xb
  ;;

let res1 = List.fold_left (fun mem (xa, xb) -> mem + (part_1_easy xb)) 0 lst;;


(*Part 2 *)

let map_line xa =
  let arr = Array.make 7 0 in (*a: 0, b: 1*)
  List.iter (fun word ->
    String.iter (fun c -> arr.(int_of_char c - 97) <- arr.(int_of_char c - 97) + 1) word
    ) xa;

  (*Filter to get the segments*)
  let a1 = Array.mapi (fun i x -> (char_of_int (i + 97), x)) arr in
  let bb = Array.fold_left (fun mem (i, x) -> if x == 6 then i else mem) ' ' a1 in
  let ee = Array.fold_left (fun mem (i, x) -> if x == 4 then i else mem) ' ' a1 in
  let ff = Array.fold_left (fun mem (i, x) -> if x == 9 then i else mem) ' ' a1 in
  let ac = Array.fold_left (fun mem (i, x) -> if x == 8 then i::mem else mem) [] a1 in
  let dg = Array.fold_left (fun mem (i, x) -> if x == 7 then i::mem else mem) [] a1 in

  let arr_nbr = Array.make 8 [] in
  List.iteri (fun i word ->
    let l = String.length word in
    arr_nbr.(l) <- word::arr_nbr.(l)
    ) xa;

    (*ac: a pas dans 4*)
    let nbr_4 = List.hd arr_nbr.(4) in
    let aa = List.fold_left (fun mem c -> if String.contains nbr_4 c then mem else c) ' ' ac
    and cc = List.fold_left (fun mem c -> if String.contains nbr_4 c then c else mem) ' ' ac in

    let gg = List.fold_left (fun mem c -> if String.contains nbr_4 c then mem else c) ' ' dg
    and dd = List.fold_left (fun mem c -> if String.contains nbr_4 c then c else mem) ' ' dg in


    [|aa; bb; cc; dd; ee; ff; gg|] (*Use int_of_char to go back to number*)
;;

let decode_word word arr_code =
  let l = String.length word in
  if l == 2 then 1
  else if l == 3 then 7
  else if l == 4 then 4
  else if l == 7 then 8
  else if l == 5 then (
    if (String.contains word arr_code.(5)) == false
    then 2 (*f not in 2*)
    else if String.contains word arr_code.(2)
    then 3
    else 5
    )
  else (*6*) (
    if String.contains word arr_code.(4) == false
    then 9 (*e not in 9 *)
    else if String.contains word arr_code.(3)
    then 6 (*d in 6*)
    else 0
    )
;;

let decode_full_line xa xb =
  let arr_code = map_line xa in
  List.fold_left (fun mem x -> mem * 10 + (decode_word x arr_code)) 0 xb
;;

List.fold_left (fun mem (xa, xb) -> mem + (decode_full_line xa xb)) 0 lst;;
