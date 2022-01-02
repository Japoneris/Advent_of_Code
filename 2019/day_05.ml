let scanint x = Scanf.sscanf x "%d " (fun y -> y);;
let transform x = Array.of_list (List.map (scanint) (String.split_on_char ',' x));;

let read_input_data () =
  let ic = open_in "input_05" in

  let stri  = String.split_on_char ',' (input_line ic) in
  close_in_noerr ic;
  Array.of_list (List.map (scanint) stri)
;;

let decompose data i =
  if data.(i) == 99
  then (99, 0, 0, 0)
  else if data.(i) == 4
  then (4, data.(data.(i+1)), 0, 0)
  else (
    let (op, inter) = (data.(i) mod 100, data.(i) / 100) in
    if (op <= 2) || (op > 4)
    then (
      let a = if inter mod 10 == 0 then data.(data.(i+1)) else data.(i+1)
      and b = if (inter / 10) mod 10 == 0 then data.(data.(i+2)) else data.(i+2)
      and c = data.(i+3) in
      (op, a, b, c)
      )
      else (
        (op, data.(i+1), 0, 0)
        )
    )
;;




let solve data inp =
  let rec nest i =
    Printf.printf "%d:" i;
    let vi = data.(i) in
    Printf.printf " %d\n" vi;
    if vi == 1
    then (
      let a = data.(data.(i+1))
      and b = data.(data.(i+2)) in
      data.(data.(i+3)) <- a + b;
      nest (i+4)
      )
    else if vi == 99
    then data
    else if vi == 2
    then (
      let a = data.(data.(i+1))
      and b = data.(data.(i+2)) in
      data.(data.(i+3)) <- a * b;
      nest (i+4)
      )
    else if vi == 3
    then (
      data.(data.(i+1)) <- inp;
      nest (i+2)
      )
    else if vi == 4
    then (
      Printf.printf "DEBUG_2: %d\n" data.(data.(i+1));
      nest (i+2)
      )
    else if vi >= 100
    then (
      let (op, a, b, c) = decompose data i in
      if op == 1
      then (
        data.(c) <- a + b;
        nest (i+4)
        )
      else if op == 2
      then (
        data.(c) <- a * b;
        nest (i+4)
        )
      else if op == 3
      then (
        data.(a) <- inp;
        nest (i+2)
        )
      else if op == 4
      then (
        Printf.printf "DEBUG_1: %d\n" a;
        nest (i+2)
        )
      else (
        Printf.printf "Error: %d\n" vi;
        data
        )
      )
    else (
      Printf.printf "Error: %d\n" vi;
      data
      )
    in nest 0
;;


let solve_2 data inp =

  let process_code op a b c i =
  if op == 1
  then (
    (*
    Printf.printf "\n==> OP + %d -> %d" data.(c) (a + b);
    Printf.printf "==%d \n" data.(c);
    *)
    data.(c) <- a + b;
    i + 4
    )
  else if op == 2
  then (
    data.(c) <- a * b;
    i + 4
    )
  else if op == 3
  then (
    data.(a) <- inp;
    i + 2
    )
  else if op == 4
  then (
    Printf.printf "DEBUG: %d\n" a;
    i + 2
    )
  else if op == 5
  then (
    if a != 0
    then b
    else i + 3
    )
  else if op == 6
  then (
    if a == 0
    then b
    else i+3
    )
  else if op == 7
  then (data.(c) <- if a < b then  1 else 0;
    i+4)
  else if op == 8
  then (
    data.(c) <- if a == b then  1 else 0;
    i+4
    )
  else if op == 99
  then (
    Printf.printf "Halting wih code 99\n";
    i + 10000
    )
  else (
    Printf.printf "Error ! with code %d\n" op;
    i + 10000
    )
  in

  let l = Array.length data in

  let rec nest i =
    Printf.printf "%d:" i;
    if i >= l then data
    else (
      let (op, a, b, c) = decompose data i in
      Printf.printf "\t%d (%d %d %d)\n" op a b c;
      let ip = process_code op a b c i in
      nest ip
      )
    in nest 0
;;

let my_input = read_input_data ();;
solve_2 my_input 1;;

let my_input = read_input_data ();;
solve_2 my_input 5;;
