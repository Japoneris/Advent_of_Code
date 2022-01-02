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

let instruction = read_file "input_08";;
let instruction_2 = Array.of_list(List.map (parse_line) instruction);;


type instr = Nop of int
  | Acc of int
  | Jmp of int
;;


let parse_line line =
  Scanf.sscanf line "%s %d" (fun x v ->
    if String.equal "nop" x
    then Nop v
    else if String.equal "acc" x
    then Acc v
    else Jmp v)
;;

let execute_code code =
  let l = Array.length code in
  let visited = Array.make l false in

  let rec nest acc i =
  if visited.(i)
  then acc
  else (visited.(i) <- true;
  match code.(i) with
  | Acc v -> nest (acc+v) (i+1)
  | Jmp v -> nest acc (i+v)
  | Nop v -> nest acc (i+1)
  )
  in nest 0 0
;;

let code_to_print = function
| Acc v -> Printf.printf "acc %d\n" v
| Jmp v -> Printf.printf "Jmp %d\n" v
| Nop v -> Printf.printf "nop %d\n" v
;;

let execute_code_print code =
  let l = Array.length code in
  let visited = Array.make l false in
  Printf.printf "=== Start with %d instruction ===\n" l;

  let rec nest acc i =
    if i >= l
    then (
      Printf.printf "Finished at %d with acc=%d" i acc;
      (acc, true)
    )
    else (
    Printf.printf "acc=%d\tline=%d\t" acc i;
    code_to_print code.(i);

    if visited.(i)
    then (acc, false)
    else (visited.(i) <- true;
    match code.(i) with
    | Acc v -> nest (acc+v) (i+1)
    | Jmp v -> nest acc (i+v)
    | Nop v -> nest acc (i+1)
  )
  )
  in nest 0 0
;;

let modify_instruct code i =
  let code2 = Array.copy code in

  match code.(i) with
  | Acc v -> code2
  | Jmp v -> (code2.(i) <- Nop v; code2)
  | Nop v -> (code2.(i) <- Jmp v; code2)
;;

let solve_part_2 code = 
  let l = Array.length code in
  let rec nest i =
    let code2 = modify_instruct code i in
    let (acc, tf) = execute_code_print code2 in
    if tf then acc
    else nest (i-1)
  in nest (l-1)
;;
