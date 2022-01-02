let read_file file =
  let ic = open_in file in
  let line = input_line ic in
  close_in_noerr ic;
  line
;;

let convert_char_to_bin c =
  if c == '0' then [0;0;0;0]
  else if c == '1' then [0;0;0;1]
  else if c == '2' then [0;0;1;0]
  else if c == '3' then [0;0;1;1]
  else if c == '4' then [0;1;0;0]
  else if c == '5' then [0;1;0;1]
  else if c == '6' then [0;1;1;0]
  else if c == '7' then [0;1;1;1]
  else if c == '8' then [1;0;0;0]
  else if c == '9' then [1;0;0;1]
  else if c == 'A' then [1;0;1;0]
  else if c == 'B' then [1;0;1;1]
  else if c == 'C' then [1;1;0;0]
  else if c == 'D' then [1;1;0;1]
  else if c == 'E' then [1;1;1;0]
  else [1;1;1;1]
;;

let convert_string stri =
  (*
  String.fold_right (fun  c mem -> (convert_char_to_bin c) @ mem) stri []
  *)
  let l0 = String.length stri in
  let rec nest i mem =
  if i < 0 then mem
  else nest (i-1) ((convert_char_to_bin stri.[i]) @ mem)
  in nest (l0-1) []
;;

let bin_to_int lst =
  List.fold_left (fun mem x -> mem * 2 + x) 0 lst
;;

let print_list lst =
  List.iter (fun x -> Printf.printf "%d" x) lst;
  print_newline ()
;;


let get_group lst =
  let rec nest mem leftover =
  if (List.length leftover) >= 5
  then (
    let (a::b::c::d::e::lst1) = leftover in
    if a == 1
    then (*Not the last*) (
      nest (e::d::c::b::mem) lst1
      )
    else
    (
      Printf.printf "Extra bit: ";
      print_list lst1;
      print_newline ();
      (bin_to_int (List.rev (e::d::c::b::mem)), lst1)
      )
    )
  else (
    Printf.printf "Error, bit not finished\n";
    (0, []))
  in nest [] lst
;;

let get_subpacket_length lst s =
  let rec nest mem left si =
  if si == 0
  then (mem, left)
  else match left with
  | [] -> (
    Printf.printf "Error, there might be some data (miss %d)\n" si;
    (mem, [])
    )
  | xi::li -> nest (mem * 2 + xi) li (si-1)
  in nest 0 lst s
;;


let extract_sublist lst n =
(*n: the number of bits to extract*)
  let rec nest mem ni left =
  if ni == 0
  then (List.rev mem, left)
  else match left with
  | [] -> (
    Printf.printf "Error, there might be some bits left\n";
    (List.rev mem, [])
    )
  | x::lx -> nest (x::mem) (ni-1) lx
  in nest [] n lst
;;



type packet =
| Nopacket
| Literal of int * int * int
| Intermediate of int * int  * packet list
;;

let rec parse_packet_v2 lst =
  if List.length lst >= 7
  then (
    let (v0::v1::v2::lst0) = lst in
    let (t0::t1::t2::lst1) = lst0 in
    let p_version = bin_to_int (v0::v1::v2::[])
    and p_type = bin_to_int (t0::t1::t2::[]) in

    if p_type == 4
    then (*Literal value*) (
      let (literal, left) = get_group lst1 in
      (*
      Printf.printf "Literal value: %d\n" literal
      End, nothing else to see*)
      (Literal (p_version, p_type, literal), left)
      (*left can be used for the hierarchical structure*)
      )
    else (*Operator*)(
      let (b::lst2) = lst1 in
      if b == 0
      then (
        let (packet_length, lst3) = get_subpacket_length lst2 15 in
        Printf.printf "Type 0: Packet length = %d\n" packet_length;
        let (sub, lst4) = extract_sublist lst3 packet_length in

        let (result, lst5) = List.fold_left (fun (m0, lstu) _ ->
          if List.length lstu < 7 then (m0, lstu)
          else let (pck, left) = parse_packet_v2 lstu in
          (pck::m0, left)
        ) ([], sub) (List.init ((List.length sub) / 7) (fun _ -> 1)) in
        (*lst5 must be negliginle*)
        (Intermediate (p_version, p_type, result), lst4)

        )
      else (
        let (packet_count, lst3) = get_subpacket_length lst2 11 in
        Printf.printf "Type 1: Packet count = %d\n" packet_count;

        let (result, lst5) = List.fold_left (fun (m0, lstu) _ ->
          let (pck, left) = parse_packet_v2 lstu in
          (pck::m0, left)
        ) ([], lst3) (List.init packet_count (fun _ -> 1)) in

        (Intermediate (p_version, p_type, result), lst5)
        )
      )
    )
  else (Nopacket, lst)
;;

let count_version tree =
  let rec nest = function
  | Nopacket -> 0
  | Literal (a, b, c) -> a
  | Intermediate (a, b, other) ->
    List.fold_left (fun mem trx -> mem + (nest trx)) a other
  in nest tree
;;



let str0 = read_file "input_16.txt";;
let bin_string = convert_string str0;;
let (r0, left) = parse_packet_v2 bin_string;;
count_version r0;;

let bin_string = convert_string "38006F45291200";;
parse_packet bin_string;;

let bin_string = convert_string "8A004A801A8002F478";;
parse_packet bin_string;;

(**)

let bin_string = convert_string "38006F45291200";;
let (r0, left) = parse_packet_v2 bin_string;;
count_version r0;;

let bin_string = convert_string "8A004A801A8002F478";;
let (r0, left) = parse_packet_v2 bin_string;;
count_version r0;;

let bin_string = convert_string "620080001611562C8802118E34";;
let (r0, left) = parse_packet_v2 bin_string;;
count_version r0;;




(*Part 2*)

let is_greater lst =
  let ll = List.length lst in
  if ll == 2 then match lst with
  | (Literal (_, _, b))::(Literal (_, _, a))::[]  ->
    if a > b then 1 else 0
  | _ -> (
    Printf.printf "Error, no match for greater\n"; 0)
  else (
    Printf.printf "Error, greater, no sufficient input (%d)\n" ll; 0)
;;

let is_less lst =
  let ll = List.length lst in
  if ll == 2 then match lst with
  | (Literal (_, _, b))::(Literal (_, _, a))::[]  ->
    if a < b then 1 else 0
  | _ -> (
    Printf.printf "Error, no match for less\n"; 0)
  else (
    Printf.printf "Error, is_less, no sufficient input (%d)\n" ll; 0)
;;

let is_equal lst =
  let ll = List.length lst in
  if ll == 2 then match lst with
  | (Literal (_, _, b))::(Literal (_, _, a))::[]  ->
    if a == b then 1 else 0
  | _ -> (
    Printf.printf "Error, no match for equal\n"; 0)
  else (
    Printf.printf "Error, is_equal, no sufficient input (%d)\n" ll; 0)
;;



let rec parse_packet_v3 lst =
  if List.length lst >= 7
  then (
    let (v0::v1::v2::lst0) = lst in
    let (t0::t1::t2::lst1) = lst0 in
    let p_version = bin_to_int (v0::v1::v2::[])
    and p_type = bin_to_int (t0::t1::t2::[]) in

    if p_type == 4
    then (*Literal value*) (
      let (literal, left) = get_group lst1 in
      (*
      Printf.printf "Literal value: %d\n" literal
      End, nothing else to see*)
      (Literal (p_version, p_type, literal), left)
      (*left can be used for the hierarchical structure*)
      )
    else (*Operator*)(
      let (b::lst2) = lst1 in
      let (result, lst_left) = (
        if b == 0
        then (
          let (packet_length, lst3) = get_subpacket_length lst2 15 in
          let (sub, lst4) = extract_sublist lst3 packet_length in
          let (result, lst5) = List.fold_left (fun (m0, lstu) _ ->
            if List.length lstu < 7 then (m0, lstu)
            else let (pck, left) = parse_packet_v3 lstu in
            (pck::m0, left)
          ) ([], sub) (List.init ((List.length sub) / 7) (fun _ -> 1)) in

          (result, lst4))
        else (
          let (packet_count, lst3) = get_subpacket_length lst2 11 in
          let (result, lst5) = List.fold_left (fun (m0, lstu) _ ->
            let (pck, left) = parse_packet_v3 lstu in
            (pck::m0, left)
          ) ([], lst3) (List.init packet_count (fun _ -> 1)) in

          (result, lst5))
        ) in
        if p_type == 0 then (*Sum*)(
          let vx = List.fold_left (fun mem x -> match x with
          | Nopacket -> mem
          | Literal (a, b, c) -> mem + c) 0 result
          in
          (Literal (p_version, p_type, vx), lst_left)
          )
        else if p_type == 1 (*Product*) then (
          let vx = List.fold_left (fun mem x -> match x with
                  | Nopacket -> mem
                  | Literal (a, b, c) -> mem * c) 1 result
                  in
                  (Literal (p_version, p_type, vx), lst_left)
                  )
        else if p_type == 2 then (
          let vx = List.fold_left (fun mem x -> match x with
                  | Nopacket -> mem
                  | Literal (a, b, c) -> min mem c) max_int result
                  in
                  (Literal (p_version, p_type, vx), lst_left)
                  )
        else if p_type == 3 then (
          let vx = List.fold_left (fun mem x -> match x with
                  | Nopacket -> mem
                  | Literal (a, b, c) -> max mem c) 0 result
                  in
                  (Literal (p_version, p_type, vx), lst_left)
                  )
        else if p_type == 5 then (
          let vx = is_greater result in
          (Literal (p_version, p_type, vx), lst_left)
                  )
        else if p_type == 6 then (
          let vx = is_less result in
          (Literal (p_version, p_type, vx), lst_left)
                  )
        else (
          let vx = is_equal result in
          (Literal (p_version, p_type, vx), lst_left)
                  )

      )
    )
  else (Nopacket, lst)
;;


let bin_string = convert_string "C200B40A82";;
let (r0, left) = parse_packet_v3 bin_string;;
(*3*)


let bin_string = convert_string "04005AC33890";;
let (r0, left) = parse_packet_v3 bin_string;;
(*54*)


let bin_string = convert_string "880086C3E88112";;
let (r0, left) = parse_packet_v3 bin_string;;
(*7*)

let bin_string = convert_string "CE00C43D881120";;
let (r0, left) = parse_packet_v3 bin_string;;
(*9*)

let bin_string = convert_string "D8005AC2A8F0";;
let (r0, left) = parse_packet_v3 bin_string;;
(*1*)

let bin_string = convert_string "F600BC2D8F";;
let (r0, left) = parse_packet_v3 bin_string;;
(*0*)


let bin_string = convert_string "9C005AC2F8F0";;
let (r0, left) = parse_packet_v3 bin_string;;
(*0*)

let bin_string = convert_string "9C0141080250320F1802104A08";;
let (r0, left) = parse_packet_v3 bin_string;;
(*0*)



let str0 = read_file "input_16.txt";;
let bin_string = convert_string str0;;
let (r0, left) = parse_packet_v3 bin_string;;
