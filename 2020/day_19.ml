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

let data = read_file "input_19";;

type rules =
  | C of char
  | R of int list
  | L of rules list
;;

let parse_rule line =
  let (a1::a2::[]) = String.split_on_char ':' line in

  let rst = if List.length (String.split_on_char '"' a2) > 1
    then Scanf.sscanf a2 " \"%c\"" (fun a -> C (a))
    else (
      let lst = String.split_on_char '|' a2 in
      if List.length lst > 1
      then (
        let lsx = List.map (fun row ->
          let lsint = String.split_on_char ' ' row in
          R (List.rev (List.fold_left (fun mem x ->
            if String.length x == 0
            then mem
            else (int_of_string x)::mem
            ) [] lsint))
            ) lst in
            L lsx
        )
      else (
        R (List.rev (List.fold_left (fun mem x ->
            if String.length x == 0
            then mem
            else (int_of_string x)::mem
          ) [] (String.split_on_char ' ' a2)))
        )
      ) in
  (int_of_string a1, rst)
;;

let list_of_rules_to_arr lst =
  let l = List.length lst in
  let a = Array.make l (R []) in
  List.iter (fun (i, b) -> a.(i) <- b ) lst;
  a
;;


let extract_rules data =
  let rec nest mem = function
  | [] -> (list_of_rules_to_arr mem, [])
  | x::lx -> (
    Printf.printf "%s\n" x;
    if String.length x == 0
    then (list_of_rules_to_arr mem, lx)
    else nest ((parse_rule x)::mem) lx
    )
  in nest [] data
;;

let check_rule rules line =
  let lmax = String.length line in

  let rec nest i = function
  | [] -> (i == lmax)
  | (C c)::lx ->
    if i >= lmax
    then false
    else if (line.[i] == c)
    then nest (i+1) lx
    else false
  | (R r)::lx -> nest i ((List.map (fun j -> rules.(j)) r)@lx)
  | (L rr)::lx -> (
    List.fold_left (fun mem (R r) ->
      mem || (nest i ((List.map (fun j -> rules.(j)) r)@lx))
      ) false rr
    ) in
  nest 0 [ R [0]]
;;

(*44 / 18 not correct*)

let (rl, other) = extract_rules data;;
List.fold_left (fun mem x -> if check_rule rl x then mem+1 else mem) 0 other;;

let data_r = read_file "input_19_test";;
let (rl_t, other_t) = extract_rules data_r;;

List.fold_left (fun mem x -> if check_rule rl_t x then mem+1 else mem) 0 other_t;;


let data_b = read_file "input_19_b";;
let (rlb, other) = extract_rules data_b;;
List.fold_left (fun mem x -> if check_rule rlb x then mem+1 else mem) 0 other;;


  let rec nest i v=
  match  rules.(i) with
  | C c -> (i+1, line.[i] == c)
  | R r -> List.fold_left (fun (ii, tf) x ->
    if tf then nest ii x
    else (ii, tf)
    ) (i, true) r

  | L lx -> List.fold_left (fun tf1 r ->
    List.fold_left (fun (ii, tf) x ->
      if tf then nest ii x
      else (ii, tf)
      ) (i, true) r
    ) lx
