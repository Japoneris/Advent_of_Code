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

let data = read_file "input_21";;

let parse_allerg line =
  let l = String.length line in
  let lineb = String.sub line 0 (l-1) in
  let linec = String.split_on_char ',' lineb in
  List.map (fun x ->
    let (_::a2::[]) = String.split_on_char ' ' x in
    a2
    ) linec
;;

let parse_line line =
  let (a1::a2::[]) = String.split_on_char '(' line in
  let ingr = String.split_on_char ' ' (String.sub a1 0 (String.length a1 - 1)) in
  (ingr, parse_allerg a2)
;;

type ingr =
| Ingredient of int
| Alerg of int
;;

let make_hash_table lst_list =
  let lst = List.flatten lst_list in
  let tbl = Hashtbl.create (List.length lst) in
  let rec nest cnt = function
  | [] -> tbl
  | x::lx -> (
    if Hashtbl.mem tbl x
    then nest cnt lx
    else (
      Hashtbl.add tbl x cnt;
      nest (cnt+1) lx
      )
    ) in nest 0 lst
;;

let convert h_tbl lst =
  let arr = Array.make (Hashtbl.length h_tbl) false in
  List.iter (fun x ->
    arr.(Hashtbl.find h_tbl x) <- true
    ) lst;
  arr
;;


let solve_1 data =
  let ingr_alerg = List.map (parse_line) data in
  let ingr = List.map (fun (x, y) -> x) ingr_alerg in
  let alerg = List.map (fun (x, y) -> y) ingr_alerg in

  let tbl_ingr = make_hash_table ingr
  and tbl_alle = make_hash_table alerg in

  (List.map (fun x -> convert tbl_ingr x) ingr,
    List.map (fun x -> convert tbl_alle x) alerg
  )
;;

(*Strategy: extract for each alergen the list where it is*)
let filter_aller lst_ingr lst_aller i_all =
  let rec nest mem = function
  | ([], []) -> mem
  | (x::lx, y::ly) -> if y.(i_all)
    then nest (x::mem) (lx, ly)
    else nest mem (lx, ly)
  in nest [] (lst_ingr, lst_aller)
;;

(*list of array, reduce to single true array*)
let check_candidate lst =
  let (a1::left) = lst in

  let rec nest mem = function
  | [] -> mem
  | a1::lx -> nest (Array.mapi (fun i x -> x && a1.(i)) mem) lx
  in nest a1 left
;;

let extract_cand_index arr =
  let (_, lst) = Array.fold_left (fun (i, mem) x ->
    if x then (i+1, i::mem) else (i+1, mem)
  ) (0, []) arr
  in lst
;;

let print_arr  row =
  Array.iter (fun x ->
    if x
    then Printf.printf "#"
    else Printf.printf " "
    ) row
    ;;

let print_list lst =
  List.iter (fun row ->
    print_arr row;
    print_newline ()
    ) lst
;;


let ingr, allers = solve_1 data;;

(*Allow to visualize*)
print_list (filter_aller ingrs allers 1);;

check_candidate (filter_aller ingrs allers 1);;
print_arr (check_candidate (filter_aller ingrs allers 1));;

extract_cand_index (check_candidate (filter_aller ingrs allers 1));;

0:  [68; 49; 15]
1:  [68; 50; 15; 8]
2:  [68; 26; 15]
3:  [68; 48; 15]
4:  [68; 48; 41; 26]
5:  [68; 15]
6:  [68; 48; 26; 15; 8]
7:  [92; 68; 49; 48; 26]



let all_alerg = [91; 50; 49; 48; 41; 26; 15; 8];;


let count_okay arr allergs =
  let c_allerg = List.fold_left (fun mem i ->
    if arr.(i) then mem+1 else mem
    ) 0 allergs
  and c_tot = Array.fold_left (fun mem x ->
    if x then mem+1 else mem
    ) 0 arr
  in c_tot - c_allerg
;;

List.fold_left (fun mem row ->
  mem + (count_okay row all_alerg)
  ) 0 ingrs;;

(*2307*)

let ingr_aller_tup = [(7, 91); (1, 50); (0, 49); (3, 48);
  (4, 41); (2, 26); (5, 15); (6, 8)];;


let get_name hs_tbl i =
  Hashtbl.fold (fun x v mem ->
    if v == i then x else mem) hs_tbl ""
;;

let extract_names hs_ingrs hs_aller lst =
  let rec nest mem = function
  | [] -> mem
  | (x, y)::lx ->
    nest ((get_name hs_ingrs x, get_name hs_aller y)::mem) lx
  in nest [] lst
;;


let solve_2 data lsx =
  let ingr_alerg = List.map (parse_line) data in
  let ingr = List.map (fun (x, y) -> x) ingr_alerg in
  let alerg = List.map (fun (x, y) -> y) ingr_alerg in

  let tbl_ingr = make_hash_table ingr
  and tbl_alle = make_hash_table alerg in

  let l_tup = extract_names tbl_alle tbl_ingr  lsx in
  let l_tup_order = List.sort (fun (a, a1) (b, b1) -> String.compare a b) l_tup in
  let l_words = List.map (fun (a, b) -> b) l_tup_order in
  String.concat "," l_words
  ;;


;;
