let scanint x = Scanf.sscanf x "%d " (fun y -> y);;
let transform x = Array.of_list (List.map (scanint) (String.split_on_char ',' x));;


let read_input_data path =
  let ic = open_in path in
  let stri  = input_line ic in
  close_in_noerr ic;
  transform stri
;;


let decompose data i base =
  if data.(i) == 99
  then (99, 0, 0, 0)
(*
  else if data.(i) == 9
  then (9, data.(i+1), 0, 0) (*No base, default is one*)
  *)
  else (
    let (op, inter) = (data.(i) mod 100, data.(i) / 100) in
    if (op <= 2) || ((op > 4) && (op < 9))
    then (
      let ga = inter mod 10
      and gb = (inter / 10) mod 10
      and gc = (inter / 100) mod 10
      in

      let a = if ga == 0
              then data.(data.(i+1))
              else if ga == 2 then data.(data.(i+1) + base)
              else data.(i+1)
      and b = if gb == 0
              then data.(data.(i+2))
              else if gb == 2 then data.(data.(i+2) + base)
              else data.(i+2)
      and c = if gc == 0  (*No 1 normaly*)
              then data.(i+3)
              else data.(i+3) + base in
      (op, a, b, c)
      )
      else if (op == 9)
      then (
        let res = if inter mod 10 == 0 then data.(data.(i+1))
                  else if inter mod 10 == 1 then data.(i+1)
                  else data.(data.(i+1) + base) in
        (9, res, 0, 0)
        )
      else if op == 4
      then (
        let res = if inter mod 10 == 0 then data.(data.(i+1))
                  else if inter mod 10 == 1 then data.(i+1)
                  else data.(data.(i+1) + base) in
        (4, res, 0, 0)
        )
      else if (op == 3)
      then (
        let res = if inter mod 10 == 0 then data.(i+1)
                  else if inter mod 10 == 1 then i+1
                  else data.(i+1) + base in
        (op, res, 0, 0))
      else (
        (op, data.(i+1), 0, 0)
        )
    )
;;



let solve_2 data_in inp l =

  let data = Array.make l 0 in
  Array.iteri (fun i x -> data.(i) <- x) data_in;


  let process_code op a b c i base =
  if op == 1
  then (
    data.(c) <- a + b;
    (i + 4, base))
  else if op == 2
  then (
    data.(c) <- a * b;
    (i + 4, base))
  else if op == 3
  then (
    data.(a) <- inp;
    (i + 2, base))
  else if op == 4
  then (
    Printf.printf "DEBUG: %d\n" a;
    (i + 2, base))
  else if op == 5
  then (
    let res = if a != 0 then b else i + 3 in
    (res, base))
  else if op == 6
  then (
    let res = if a == 0 then b else i+3
    in (res, base))
  else if op == 7
  then (data.(c) <- if a < b then  1 else 0;
    (i+4, base))
  else if op == 8
  then (data.(c) <- if a == b then  1 else 0;
    (i+4, base)
    )
  else if op == 9
  then (
    (i + 2, base + a)
    )
  else if op == 99
  then (
    Printf.printf "Halting wih code 99\n";
    (i + l*2, base)
    )
  else (
    Printf.printf "Error ! with code %d\n" op;
    (i + l*2, base)
    )
  in

  let rec nest i base =
    if i >= l then data
    else (
      Printf.printf "%d: \t(B %d) \t(dec: %d %d %d %d)\n" i base data.(i) data.(i+1) data.(i+2) data.(i+3);
      let (op, a, b, c) = decompose data i base in
      Printf.printf "\t%d (%d %d %d)\n" op a b c;
      let (ip, base_n) = process_code op a b c i base in
      nest ip base_n
      )
    in nest 0 0
;;


let data = read_input_data "input_09";;
let data_test = transform "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99";;
let data_test_2 = transform "1102,34915192,34915192,7,4,7,99,0";;
let data_test_3 = transform  "104,1125899906842624,99";;

let data_05 = read_input_data "input_05";;
