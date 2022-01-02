let data_groups = read_file "input_06";;

let group_items lst =
  let rec nest mem x = function
  | [] -> List.map (String.concat "") (x::mem)
  | y::ly -> if String.equal y ""
  then nest (x::mem) [] ly
  else nest mem (y::x) ly
  in nest [] [] lst
;;


let count_question_raw x =
  let arr = Array.make 26 0 in
  String.iter (fun c ->
    arr.(Char.code c - 97) <- 1
    ) x;
  arr
;;

let count_question x =
  let arr = count_question_raw x in
  Array.fold_left (+) 0 arr
;;

let groups = group_items data_groups;;

List.fold_left (fun mem x -> mem + count_question x) 0 groups;;


let group_items_2 lst =
  let rec nest mem x = function
  | [] -> x::mem
  | y::ly -> if String.equal y ""
  then nest (x::mem) [] ly
  else nest mem (y::x) ly
  in nest [] [] lst
;;




let count_question_2 lst =
  let a2 = List.fold_left (fun a0 x ->
    let a1 = count_question_raw x in
    Array.mapi (fun i x -> x * a0.(i)) a1
    ) (Array.make 26 1) lst in
    Array.fold_left (+) 0 a2
;;

let groups = group_items_2 data_groups;;
List.fold_left (fun mem x -> mem + count_question_2 x) 0 groups;;
