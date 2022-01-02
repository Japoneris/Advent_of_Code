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


let bag_lines = read_file "input_07";;

type bag = {thebag: string; bag_list: (int * string) list};;

let concat_space lst_str =
  String.concat " " (List.rev lst_str)
;;

let concat_space_1 lst_str =
  let l1 = List.rev lst_str in
  let a1 = List.hd l1 in
  if String.equal a1 "no"
  then (0, "")
  else (int_of_string a1 , String.concat " " (List.tl l1))
  ;;


let transform_bag_list lst_words =

  let rec nest mem mem1 mem_tmp = function
  | [] ->
    if (List.length mem1 == 1)
    then (
      let (a, b)::[] = mem1 in
      if a == 0
      then {thebag=mem; bag_list=[]}
      else {thebag=mem; bag_list=mem1}
      )
    else
      {thebag= mem;
      bag_list = mem1
    }
  | x::lx ->
    (if (String.equal x "contains") || (String.equal x "contain")
    then nest mem [] []
    else if (String.equal x "bags") || (String.equal x "bag")
    then (
      if String.length mem == 0
      then nest (concat_space mem_tmp) [] []
      else nest mem ((concat_space_1 mem_tmp)::mem1) []
      )
    else nest mem mem1 (x::mem_tmp)
    ) lx
  in nest "" [] [] lst_words
;;
(*fill bag struct*)
let parse_bag_line line =
  let l = String.length line in
  let hashed_line = String.split_on_char ',' (String.sub line 0 (l-1)) in
  let line1 = String.concat "" hashed_line in
  transform_bag_list (String.split_on_char ' ' line1)
;;

let bag_data = List.map (parse_bag_line) bag_lines;;

let check lst =
  if List.length lst == 0
  then 0
  else
  let rec nest = function
  | [] -> 2
  | (a, b)::lx ->
    if String.equal b "shiny gold"
    then 1
    else nest lx
  in nest lst
;;

let bag_arr = Array.of_list bag_data;;

let arr_tf = Array.map (fun x ->
  check x.bag_list
) bag_arr;;

let search_index bag_arr bag_type =
  let rec nest i =
  if String.equal bag_arr.(i).thebag bag_type
  then i
  else nest (i+1)
  in nest 0
;;

let search_value a_tf a_names bag =

  let rec nest bi =
    let i = search_index a_names bi in
    if a_tf.(i) == 0
    then 0
    else if a_tf.(i) == 1
    then 1
    else (
      let result = List.fold_left (fun mem (_,x) ->
        max (nest x) mem
        ) 0 a_names.(i).bag_list in
      a_tf.(i) <- result;
      result)
  in nest bag
;;

search_value arr_tf bag_arr "dim coral";;

Array.map (fun bag -> search_value arr_tf bag_arr bag.thebag) bag_arr;;

Array.fold_left (+) 0 arr_tf;;

(*Part 2*)


let count_bellow_bags a_names =
  let rec nest bi =
    let i = search_index a_names bi in
    List.fold_left (fun mem (a,b) ->
      mem + a * (nest b)
    ) 1 a_names.(i).bag_list
  in nest "shiny gold"
;;
