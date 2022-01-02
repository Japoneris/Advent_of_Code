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

let data = read_input_data "input_25";;




let solve data_in l message =
  let data = Array.make l 0 in
  Array.iteri (fun i x -> data.(i) <- x) data_in;

  let l1 = String.length message in
  let cnt = ref 0 in
  let mess_tmp = ref "" in
  let cnt_1 = ref 0 in


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

      let b = if !cnt < l1
      then (
        cnt := !cnt + 1;
        int_of_char message.[!cnt -1]
        )
      else if ((!cnt_1) < String.length (!mess_tmp))
      then (
        cnt_1 := !cnt_1 + 1;
        int_of_char (!mess_tmp.[!cnt_1 -1])
        )
      else (
        Printf.printf "INPUT: %d " a;
        print_newline ();
        mess_tmp := String.concat "" [(read_line ()); "\n"];

        cnt_1 := 1;
        int_of_char !mess_tmp.[0]
        (*
        read_int ()
        *)
      ) in
      data.(a) <- b;

    (i + 2, base))
  else if op == 4
  then (

    let _ = if a < 128
    then Printf.printf "%c" (char_of_int a)
    else Printf.printf "%d, " a in

    (i + 2, base))
  else if op == 5
  then (let res = if a != 0 then b else i + 3 in (res, base))
  else if op == 6
  then (
    let res = if a == 0 then b else i+3
    in (res, base))
  else if op == 7
  then (data.(c) <- if a < b then  1 else 0; (i+4, base))
  else if op == 8
  then (data.(c) <- if a == b then  1 else 0; (i+4, base))
  else if op == 9
  then (i + 2, base + a)
  else if op == 99
  then (
    Printf.printf "Halting wih code 99\n";
    (i + l*2, base))
  else (
    Printf.printf "Error ! with code %d\n" op;
    (i + l*2, base))
  in


  let rec nest i base =
    if i >= l then data
    else (
      let (op, a, b, c) = decompose data i base in
      let (ip, base_n) = process_code op a b c i base in
      nest ip base_n
      )
    in nest 0 0
;;

let message = "north
south
south
take space heater
south
east
take loom
west
north
north
north
north
take sand
south
west
east
east
south
";;
solve data 10000 message;;

take molten lava
take infinite loop
