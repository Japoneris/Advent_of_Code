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


let int_to_binary x =
  Array.init 36 (fun i ->
    (x lsr i) land 1
    );;

let binary_to_int arr =
  let a1 = Array.mapi (fun i x -> x lsl i) arr in
  Array.fold_left (+) 0 a1;;

let update_mask mask v =
  let vbin = int_to_binary v in
  Array.mapi (fun i x ->
    if mask.[35-i] == '1'
    then 1
    else if mask.[35-i] == '0'
    then 0
    else x
    ) vbin
;;



type instruction =
| Mask of string
| Inst of (int * int)
;;

type memory = {idx: int; value: int array};;

let parse_line line =
  if String.equal (String.sub line 0 3) "mem"
  then
    Scanf.sscanf line "mem[%d] = %d" (fun x y -> Inst (x, y))
  else
    Scanf.sscanf line "mask = %s" (fun x -> Mask x)
;;

let search_item lst id =
  let rec nest mem = function
  | [] -> ({idx=id; value=Array.make 36 0}, mem)
  | item::lx ->
    if item.idx == id
    then (item, lx@mem)
    else nest (item::mem) lx
  in nest [] lst
;;


let solve_1 inst =
  let rec nest lst msk = function
  | [] -> List.fold_left (fun mem x -> mem + binary_to_int x.value) 0 lst
  | (Inst (x, y))::lx -> (
    let (item, lst_new) = search_item lst x in
    let new_item = {idx=x; value=update_mask msk y} in
    nest (new_item::lst_new) msk lx
    )
  | (Mask stri)::lx -> nest lst stri lx
  in nest [] "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" inst
;;

let results_1 = solve_1 (List.map (parse_line) data);;


type bits =
| B of int
| F
;;

type memory2 = {idx: int; v: int};;


let update_mask_2 mask v =
  let vbin = int_to_binary v in
  Array.mapi (fun i x ->
    if mask.[35-i] == '1'
    then B 1
    else if mask.[35-i] == '0'
    then B x
    else F
    ) vbin
;;

let search_item2 lst id =
  let rec nest mem = function
  | [] -> ({idx=id; v=0}, mem)
  | item::lx ->
    if item.idx == id
    then (item, lx@mem)
    else nest (item::mem) lx
  in nest [] lst
;;


(*
Difference of approach
- For each insturction: search the corresponding address

*)

let to_address x msk =
  (*Mask array *)
  let a = update_mask_2 msk x in
  let rec nest address i =
  if i == 36
  then address
  else match  a.(i) with
  | B b -> nest (List.map (fun y -> (b lsl i) + y) address) (i+1)
  | F -> nest ((List.map (fun y -> (1 lsl i) + y) address)@address) (i+1)
  in nest [0] 0
;;


let solve_2 inst =
  let rec nest lst msk = function
  | [] -> lst
  | (Inst (x, y))::lx -> (
    (*Waiting time*)
    Printf.printf "%d" (List.length lx);
    print_newline ();
    let addresses = to_address x msk in
    let lst_correct = List.fold_left (fun mem add ->
      let (item, lst_new) = search_item2 mem add in
      let new_item = {idx=add; v=y} in
      new_item::lst_new
      ) lst addresses in
    nest lst_correct msk lx
    )
  | (Mask stri)::lx -> nest lst stri lx
  in nest [] "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" inst
;;


let results_2 = solve_2 (List.map (parse_line) data);;
List.fold_left (fun mem x -> mem + x.v) 0 results_2;;


let check_values val2 =
  let rec nest i =
  if i == 36
  then 0
  else match val2.(i) with
  | F -> (1 lsl i) + 2* nest (i+1)
  | B x -> (x lsl i) + nest (i+1)
  in nest 0
;;


(*too low 53722733581*)
