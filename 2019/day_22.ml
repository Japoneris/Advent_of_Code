let scanint x = Scanf.sscanf x "%d " (fun y -> y);;

type action = Newstack | Increment of int |   Cut of int;;

let process_line line =
  let words = Array.of_list (String.split_on_char ' ' line) in
  let l = Array.length words in
  if l == 2
  then Cut (scanint words.(1))
  else if String.contains words.(3) 's'
  then Newstack
  else Increment (scanint words.(3))
;;


let read_input_data path =
  let ic = open_in path in

  let rec nest lst =
  try (
    let line = input_line ic in
    nest ((process_line line)::lst)
    )
  with e ->
    (close_in_noerr ic; List.rev lst)

  in nest []
;;


let string_test = String.split_on_char '\n' "deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1"
;;

let string_test = String.split_on_char '\n' "cut 6
deal with increment 7
deal into new stack"
;;

let string_test = String.split_on_char '\n' "deal with increment 7
deal with increment 9
cut -2"
;;

let string_test = String.split_on_char '\n' "deal with increment 7
deal into new stack
deal into new stack"
;;

let data_test = List.map (process_line) string_test;;

let data = read_input_data "input_22";;

let deal_new_stack deck =
  List.rev deck
;;

(*Not the right function
let deal_increment deck ic =
  let rec nest mem i = function
  | ([], []) -> List.rev mem
  | (stc, x::lx) ->
    if i mod ic == 0
    then nest (x::mem) (i+1) (stc, lx)
    else nest mem (i+1) (x::stc, lx)
  | (stc, []) -> nest mem i ([], List.rev stc)
  in nest [] 0 ([], deck)
;;

let deal_increment deck ic =
  let n = List.length deck in
  let arr = Array.make n (-1) in

  let rec nest i j = function
  | [] -> Array.to_list arr
  | x::lx ->
    if (arr.(i) == -1) && (j mod ic == 0)
    then (arr.(i) <- x; nest ((i+1) mod n) (j+1) lx)
    else if (arr.(i) == -1)
    then nest ((i+1) mod n) (j+1) (x::lx)
    else nest ((i+1) mod n) j (x::lx)
  in nest 0 0 deck
;;
*)


let deal_increment deck ic =
  let n = List.length deck in
  let arr = Array.make n (-1) in

  let rec nest i = function
  | [] -> Array.to_list arr
  | x::lx ->
    if (i mod ic == 0)
    then (arr.(i mod n) <- x; nest (i+1) lx)
    else nest (i+1) (x::lx)
  in nest 0 deck
;;



let cut deck n =
  let ntot = List.length deck in
  let neff = (n+ntot) mod ntot in (*Avoid negative values*)

  let rec nest mem i = function
  | x::lx -> if i == neff
    then (x::lx) @ (List.rev mem)
    else nest (x::mem) (i+1) lx
  | [] -> List.rev mem
  in nest [] 0 deck
;;



let solve n instruction =
  let cards = List.init n (fun i -> i) in

  let rec nest deck = function
  | [] -> deck
  | (Newstack)::lst -> nest (deal_new_stack deck) lst
  | (Cut x)::lst -> nest (cut deck x) lst
  | (Increment x)::lst -> nest (deal_increment deck x) lst
  in nest cards instruction
;;



let results = solve 10007 data;;

List.iteri (fun i j ->
  if j == 2019
  then Printf.printf "%d:\t%d\n" i j) results;;



(*Too high: 9687, 6097, 3270*)

let verify lst =
  let n = List.length lst in
  let arr = Array.make n (-1) in
  List.iter (fun x -> arr.(x) <- arr.(x)+1) lst;
  Array.fold_left (fun mem x -> if x != 0 then mem +1 else mem) 0 arr
  ;;


let rev_pos n i =
  n - i - 1
;;
let rev_cut n i c =
  (i + c + n) mod n
;;

let rev_increment n i c =
  let rec nest k =
  if (i + n*k) mod c == 0
  then (i+n*k)/c
  else nest (k+1)
  in nest 0
;;

let solve_reverse instruction p_end n =

  let rec nest pi = function
  | [] -> pi
  | (Newstack)::lst -> nest (rev_pos n pi) lst
  | (Cut x)::lst -> nest (rev_cut n pi x) lst
  | (Increment x)::lst -> nest (rev_increment n pi x) lst
  in nest p_end (List.rev instruction)
;;

let facto = 119315717514047;;
let nx    = 101741582076661;;


let apply f k instruction pend kmax period =
  let rec nest i pi =
  if (pi == pend) || (i mod period == 0)
  then (
    Printf.printf "%d \t %d" i pi;
    print_newline ();
    );

  if i == kmax
  then ()
  else nest (i+1) (solve_reverse instruction pi f)
  in nest 0 pend
;;


(*Too high 89412986039337*)
let tentative_memo f instruction vmax =
  let lst = List.init vmax (fun i -> solve_reverse instruction i f) in
  List.iteri (fun i j -> Printf.printf "%d \t %d" i j; print_newline ()) lst
;;

tentative_memo facto instruction 1000;;

Array.init
