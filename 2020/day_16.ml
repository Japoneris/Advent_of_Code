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


type train_detail = {i:int; j:int};;

let data = read_file "input_16";;

let parse_1 line =
  let l1::l2::[] = String.split_on_char ':' line in
  Scanf.sscanf l2 " %d-%d or %d-%d" (fun x0 y0 x1 y1 -> (l1, {i=x0; j=y0}, {i=x1; j=y1}))
;;

let parse_1b line =
  let l1::l2::[] = String.split_on_char ':' line in
  Scanf.sscanf l2 " %d-%d or %d-%d" (fun x0 y0 x1 y1 -> {i=x0; j=y0}::{i=x1; j=y1}::[])
;;


let parse_2 line =
  let lst = String.split_on_char ',' line in
  List.map (int_of_string) lst
;;

let extract_range_fields data =
  let rec nest mem = function
  | [] -> (mem, [])
  | line::lx -> (
    if String.length line == 0
    then (mem, lx)
    else (
      nest ((parse_1b line)@mem) lx
      )
    ) in nest [] data
  ;;

let extract_range_fields_2 data =
  let rec nest mem = function
  | [] -> (mem, [])
  | line::lx -> (
    if String.length line == 0
    then (mem, lx)
    else (
      nest ((parse_1b line)::mem) lx
      )
    ) in nest [] data
  ;;



let is_valid v fields =
  let rec nest = function
  | [] -> false
  | x::lx -> if (v >= x.i) && (v <= x.j)
    then true
    else nest lx
  in nest fields
;;

let solve_1 data =
  let (ticket_fields, others) = extract_range_fields data in
  let _::my::_::_::other = others in
  let my_ticket = parse_2 my in
  let other_tickets = List.map (parse_2) other
  in

  let rec nest error = function
  | [] -> error
  | x::lx -> (
    if is_valid x ticket_fields
    then nest error lx
    else nest (error + x) lx
    ) in nest 0 (List.flatten other_tickets)
;;


let is_valid_ticket ticket fields =
  let rec nest = function
  | [] -> true
  | x::lx -> if is_valid x fields
  then nest lx
  else false
  in nest ticket
;;


let reshape_fields fields =
  let rec nest mem = function
  | [] -> mem
  | x::[] -> mem
  | x::y::lx -> nest ((x, y)::mem) lx
  in nest [] fields
;;

let solve_2 data =
  let (ticket_fields, others) = extract_range_fields data in
  let _::my::_::_::other = others in
  let my_ticket = parse_2 my in
  let other_tickets = List.map (parse_2) other  in
  let fields = reshape_fields ticket_fields in

  let valid_tickets = List.fold_left (fun mem x ->
    if is_valid_ticket x ticket_fields
    then x::mem
    else mem
    ) [] other_tickets in

  fields, valid_tickets
;;

let check_position ticket fields =
  let rec nest i = function
  | [] -> ()
  | x::lx ->
  Printf.printf "\n%d match:" i;
    List.iteri (fun j (fi, fj) ->
    if is_valid x (fi::fj::[])
    then Printf.printf "%d, " j;
    ) fields;
    nest (i+1) lx
  in nest 0 ticket
;;

let check_position ticket fields =
  let rec nest mem_i = function
  | [] -> Array.of_list (List.rev mem_i)
  | x::lx ->
  let (unvalid,_) = List.fold_left (fun (mem, j) (fi, fj) ->
    if is_valid x (fi::fj::[])
    then (mem, j+1)
    else (j::mem, j+1)
    ) ([], 0) fields in

    nest (unvalid::mem_i) lx
  in nest [] ticket
;;

let all_invalid = List.map (fun x -> check_position x fil) ticks;;

List.sort (-) (List.fold_left (fun mem x -> x.(0)@mem) [] all_invalid);;
