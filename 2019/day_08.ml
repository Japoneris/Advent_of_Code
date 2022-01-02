
let read_input_data path =
  let ic = open_in path in
  let stri  = input_line ic in
  close_in_noerr ic;
  stri
;;

let data = read_input_data "input_08";;
String.length data;;
(*15000*)

let scanint x = Scanf.sscanf x "%d " (fun y -> y);;



let inp_test = "123456789012";;

let decompose_image_layer y x data =
  let l = (String.length data) / (x*y) in
  Array.init l (fun i ->
    String.sub data (i*x*y) (x*y)
  );;

let count_pxx data =
  let l = String.length data in
  Array.fold_left (fun (a,b,c) x ->
    if x == '0' then (a+1, b, c)
    else if x== '1' then (a, b+1, c)
    else (a, b, c+1)
  ) (0,0,0) (Array.init l (fun i -> data.[i]))
;;


let layer = decompose_image_layer 6 25 data;;
let result = Array.map (count_pxx) layer;;
Array.sort (fun (a0, b0, c0) (a1, b1, c1) -> a0 - a1) result;;
Printf.printf "%d\n" (let (_, b, c) = resul.(0) in b * c);;

(*Part 2*)

let reshape_layer x y layer =
  Array.init y (fun j -> Array.init x (fun i ->
    layer.[i + j*x]
    ));;

let get_color row =
  Array.fold_left (fun mem v ->
    if mem != '2' then mem
    else v
    ) '2' row
;;

let part_2 layers x y =
  let lays = Array.map (fun dat -> reshape_layer x y dat) layers in
  let l = Array.length layers in

  let get_row j i =
    let row = Array.init l (fun k -> lays.(k).(j).(i)) in
    get_color row
  in

  Array.init y (fun j -> Array.init x (fun i ->
    get_row j i )
  );;

let print_result result =
  Array.iter (fun dat ->
    Array.iter (fun x -> if x == '0'
    then Printf.printf "."
    else Printf.printf "|" ) dat;
    print_newline()
    ) result
;;


(*
0110011110100100011010001
1001000010101000001010001
1000000100110000001001010
1011001000101000001000100
1001010000101001001000100
0111011110100100110000100
*)
