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

let data = read_input_data "input_23";;

type packet = {x:int; y:int};;


let solve data_in address l =
  let data = Array.make l 0 in
  Array.iteri (fun i x -> data.(i) <- x) data_in;

  let cnt = ref 0 in
  let memory = [|-1;-1;-1|] in
  let network = Array.make 50 [] in
  let bf = ref false in

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

    let _ = if !bf == false
    then (
      data.(a) <- address;
      bf := true;
      )
    else (
      Printf.printf "INPUT: %d " a;
      print_newline ();
      let b = read_int () in
      data.(a) <- b;
      ) in

    (i + 2, base))
  else if op == 4
  then (

    memory.(!cnt mod 3) <- a;
    cnt := !cnt + 1;

    if !cnt mod 3 == 0
    then (
      Printf.printf "SEND packet to \t%d \tX=%d \tY=%d" memory.(0) memory.(1) memory.(2);
      print_newline ();
      network.(memory.(0)) <- {x=memory.(1); y=memory.(2)}::network.(memory.(0))
      );

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
      (*
      Printf.printf "%d: \t(B %d) \t(dec: %d %d %d %d)\n" i base data.(i) data.(i+1) data.(i+2) data.(i+3);
      Printf.printf "\t%d (%d %d %d)\n" op a b c;
      *)
      let (op, a, b, c) = decompose data i base in
      let (ip, base_n) = process_code op a b c i base in
      nest ip base_n
      )
    in nest 0 0
;;



let engine data_in address l lst_info =
  let data = Array.make l 0 in
  Array.iteri (fun i x -> data.(i) <- x) data_in;

  let cnt = ref 0 in
  let memory = [|-1;-1;-1|] in
  let network=  [|[]|] in
  let bf = ref false in

  let lref = ref lst_info in
  let break = ref false in


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

    let _ = if !bf == false
    then (
      (*setting network address*)
      data.(a) <- address;
      bf := true;
      )
    else if List.length !lref > 0
    then (
      let (b::lx) = !lref in
      data.(a) <- b;
      lref := lx
      )
    else (break := true) in

    (i + 2, base))
  else if op == 4
  then (

    memory.(!cnt mod 3) <- a;
    cnt := !cnt + 1;

    if !cnt mod 3 == 0
    then (
      (*
      Printf.printf "SEND packet to \t%d \tX=%d \tY=%d" memory.(0) memory.(1) memory.(2);
      print_newline ();
      *)
      network.(0) <- (memory.(0), {x=memory.(1); y=memory.(2)})::network.(0)
      );

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
    (i + l*2, base))
  else (
    Printf.printf "Error ! with code %d\n" op;
    (i + l*2, base))
  in

  let rec nest i base =
    if i >= l then List.rev(network.(0))
    else if !break then List.rev(network.(0))
    else (
      (*
      Printf.printf "%d: \t(B %d) \t(dec: %d %d %d %d)\n" i base data.(i) data.(i+1) data.(i+2) data.(i+3);
      Printf.printf "\t%d (%d %d %d)\n" op a b c;
      *)
      let (op, a, b, c) = decompose data i base in
      let (ip, base_n) = process_code op a b c i base in
      nest ip base_n
      )
    in nest 0 0
;;



let send_to_receive send =
  let receive = Array.make 50 [] in
  Array.iteri (fun i lst ->
    List.iter (fun (m, dat) ->
      receive.(m) <- (i, dat)::receive.(m)
    ) lst
  ) send;
  Array.map (fun x -> List.rev x) receive
;;

let print_received arr send =
  Array.iteri (fun i lst ->
    Printf.printf "Machine %d received %d packets and sent %d:\n" i (List.length lst) (List.length send.(i));
    List.iter (fun (a, ob) -> Printf.printf "\tFrom %d \t(X=%d, \tY=%d)\n" a ob.x ob.y) lst;
    print_newline ()
  ) arr
;;


let send_0 = Array.init 50 (fun i -> engine data i  100000 [-1])
let receive_0 = send_to_receive send_0;;

let check_all_combination i mess =




let tot_length arr =
  Array.fold_left (fun mem lst -> mem + List.length lst) 0 arr;;


let check_machine receive ni =
  let l1 = (List.flatten (List.map (fun (_, ob) -> [ob.x; ob.y]) (List.rev (receive.(ni)))))@[-1] in
  engine data ni  100000 l1
;;





let send_1 = Array.init 50 (fun i -> check_machine receive_0 i);;
let receive_1 = send_to_receive send_1;;


let update ni mess =
  let res = engine data ni  100000 mess in
  let l1 = List.flatten (List.map (fun (_, ob) -> [ob.x; ob.y]) res) in
  mess@l1@[-1]
;;





let loop n =
  let reception = Array.make 50 [-1] in

  let rec nest r =
  if r == n
  then reception
  else (
    Printf.printf "Checking machine values";
    print_newline ();
    let sent = Array.mapi (fun i mess ->
      Printf.printf "\tMachine %d" i;
      print_newline ();
      print_newline ();
      let l1 =  engine data i  100000 mess in

      List.iter (fun (m, obj) -> Printf.printf "\t\t%d: (%d, %d) \n" m obj.x obj.y) l1;
      l1
      ) reception in

    Printf.printf "Exchanging values";
    print_newline ();

    let rece = send_to_receive sent in

    Printf.printf "Updating values";
    print_newline ();
    Array.iteri (fun i xx ->
      let l1 = List.flatten (List.map (fun (_, ob) -> [ob.x; ob.y]) xx) in
      reception.(i) <-  l1 @ [-1]
      ) rece;

    nest (r+1)
    )
  in nest 0
;;



let send_to_receive_2 send =
  let receive = Array.make 50 [] in
  let nat = [|[]|] in

  Array.iteri (fun i lst ->
    List.iter (fun (m, dat) ->
      if m == 255
      then nat.(0) <- (i, dat)::nat.(0)
      else receive.(m) <- (i, dat)::receive.(m)
    ) lst
  ) send;

  let aa = Array.map (fun x -> List.rev x) receive in
  (nat, aa)

;;



let loop_2 n =
  let reception = Array.make 50 [-1] in

  let rec nest r =
  if r == n
  then reception
  else (
    (**)
    Printf.printf "Checking machine values at STEP %d" r ;
    print_newline ();
    let sent = Array.mapi (fun i mess ->
      let l1 =  engine data i  100000 mess in
      (*
      Printf.printf "\tMachine %d" i;
      print_newline ();
      List.iter (fun (m, obj) -> Printf.printf "\t\t%d: (%d, %d) \n" m obj.x obj.y) l1;
      print_newline ();
      *)

      l1
      ) reception in
(*
    Printf.printf "Exchanging values";
    print_newline ();
    Printf.printf "NAT: %d values\n" (List.length nat.(0));
    *)

    let (nat, rece) = send_to_receive_2 sent in

    let c = (tot_length reception) in
    (*

    Printf.printf "Updating values, %d message" c;
    print_newline ();
    *)
    Array.iteri (fun i xx ->
      let l1 = List.flatten (List.map (fun (_, ob) -> [ob.x; ob.y]) xx) in
      reception.(i) <-  l1 @ [-1]
      ) rece;

    let c1 = tot_length reception in
    if c == c1
    then  (
      print_newline ();
      let (_, x0)::_ = nat.(0) in
      Printf.printf "IDLE, %d message\t %d %d" c1 x0.x x0.y;
      print_newline ();
      print_newline ();

      reception.(0) <- reception.(0) @ [x0.x; x0.y; -1]
      );

    nest (r+1)
    )
  in nest 0
;;


let results = loop_2 100;;


(*Computer 0 receive stuff*)
