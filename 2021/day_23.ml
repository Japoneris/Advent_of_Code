type amphoid =
  | Amber
  | Bronze
  | Copper
  | Desert
  | Empty (*to signal place is available*)
;;

type move =
  | Out of int (*go out*)
  | In of int  (*garage*)
;;

(*
  0.1.2.3.4.5.6.7.8.9.10
# . . . . . . . . . .  . #
###### ### ### ### #######
    ## ### ### ### #
    ## ### ### ### #
    ################
*)


(*verify if the lists are full of good items or not*)

let check_columns (la, lb, lc, ld) =
  let va = List.fold_left (fun mem x ->
    match x with
    | Amber -> mem
    | _ -> false
    ) true la in
  let vb = List.fold_left (fun mem x ->
    match x with
    | Bronze -> mem
    | _ -> false
    ) true lb in
  let vc = List.fold_left (fun mem x ->
    match x with
    | Copper -> mem
    | _ -> false
    ) true lc in
  let vd = List.fold_left (fun mem x ->
    match x with
    | Desert -> mem
    | _ -> false
    ) true ld in
  (va, vb, vc, vd)
;;

let is_finished (ta, tb, tc, td) arr =
  if ta && tb && tc && td
  then Array.fold_left (fun mem x -> (x == Empty) && mem) true arr
  else false
;;


(*arr: occupied area
lst: list of items, use only the first one
i: position in the array to put stuff
*)

let rec push_left arr i =
  if (i < 0) || (i > 10) then []
  else match arr.(i) with
  | Empty -> push_left arr (i-1) @ (
    if (i !=2) && (i !=4) && (i !=6) && (i !=8) then [Out i] else []
    )
  | _ -> []
;;

let rec push_right arr i =
  if (i < 0) || (i > 10) then []
  else match arr.(i) with
  | Empty -> push_right arr (i+1) @ (
    if (i !=2) && (i !=4) && (i !=6) && (i !=8) then [Out i] else []
    )
  | _ -> []
;;


(*Search the available position near the outside of a pile*)
(*Amber, Bronze, ...: list where it is going out*)
let move_out arr tpt =
  match tpt with
  | Amber -> (push_left arr 2) @ (push_right arr 2)
  | Bronze -> (push_left arr 4) @ (push_right arr 4)
  | Copper -> (push_left arr 6) @ (push_right arr 6)
  | Desert -> (push_left arr 8) @ (push_right arr 8)
  | Empty -> (Printf.printf "Error in move out, this case must never happen\n"; [])
;;




let rec search_left arr i tpt =
  if (i < 0) then []
  else if arr.(i) == Empty
  then search_left arr (i-1) tpt
  else if arr.(i) == tpt
  then [In i]
  else [] (*Something else on the area*)
;;

let rec search_right arr i tpt =
  if (i > 10) then []
  else if arr.(i) == Empty
  then search_right arr (i+1) tpt
  else if arr.(i) == tpt
  then [In i]
  else []
;;

let move_in arr tpt =
  match tpt with
  | Amber  -> (search_right arr 3 Amber) @ (search_left arr 1 Amber)
  | Bronze -> (search_right arr 5 Bronze) @ (search_left arr 3 Bronze)
  | Copper -> (search_right arr 7 Copper) @ (search_left arr 5 Copper)
  | Desert -> (search_right arr 9 Desert) @ (search_left arr 7 Desert)
  | Empty -> (Printf.printf "Error in move in, this case must never happen\n"; [])
;;



(*Be careful to filter the stops first*)
let get_factor = function
  | Amber  -> 1
  | Bronze -> 10
  | Copper -> 100
  | Desert -> 1000
  | Empty -> (Printf.printf "Error in get factor, must not ask\n"; 0)
;;


let update_input arr pzl score tpt mv =
  let a1 = Array.init (Array.length arr) (fun i -> arr.(i)) in

  let (la, lb, lc, ld) = pzl in

  match mv with
  | In i -> (
    a1.(i) <- Empty;
    match tpt with
    | Amber -> (a1, (Amber::la, lb, lc, ld), score + abs (i - 2))
    | Bronze -> (a1, (la, Bronze::lb, lc, ld), score + 10 * abs (i - 4))
    | Copper -> (a1, (la, lb, Copper::lc, ld), score + 100 * abs (i - 6))
    | Desert -> (a1, (la, lb, lc, Desert::ld), score + 1000 * abs (i - 8))
    )
  | Out i -> (*Take something out of the list to put it away*)(
    match tpt with
    | Amber  -> (
      a1.(i) <- List.hd la;
      (a1, (List.tl la, lb, lc, ld), score + (get_factor a1.(i)) * abs (i - 2))
      )
    | Bronze -> (
      a1.(i) <- List.hd lb;
      (a1, (la, List.tl lb, lc, ld), score + (get_factor a1.(i)) * abs (i - 4))
      )
    | Copper -> (
      a1.(i) <- List.hd lc;
      (a1, (la, lb, List.tl lc, ld), score + (get_factor a1.(i)) * abs (i - 6))
      )
    | Desert -> (
      a1.(i) <- List.hd ld;
      (a1, (la, lb, lc, List.tl ld), score + (get_factor a1.(i)) * abs (i - 8)))
    )
;;


(*
input: puzzle: four list for each columns
output: list of steps that helps to solve the puzzle
*)


let print_arr arr =
  Array.iter (fun x ->
    match x with
    | Amber -> Printf.printf "A"
    | Bronze -> Printf.printf "B"
    | Copper -> Printf.printf "C"
    | Desert -> Printf.printf "D"
    | _ -> Printf.printf " ") arr;
    print_newline ()
;;

let tpt_to_letter = function
| Amber ->  'A'
| Bronze -> 'B'
| Copper -> 'C'
| Desert -> 'D'
| Empty -> ' '
;;

let print_full arr pzl =
  let (la, lb, lc, ld) = pzl in
  let sa = List.length la
  and sb = List.length lb
  and sc = List.length lc
  and sd = List.length ld in

  Printf.printf "#############\n#";
  Array.iter (fun x ->  Printf.printf "%c" (tpt_to_letter x)) arr;
  Printf.printf "#\n###%c#%c#%c#%c###\n" (if sa == 4 then tpt_to_letter (List.hd la) else ' ')
    (if sb == 4 then tpt_to_letter (List.hd lb) else ' ')
    (if sc == 4 then tpt_to_letter (List.hd lc) else ' ')
    (if sd == 4 then tpt_to_letter (List.hd ld) else ' ');

  Printf.printf "  #%c#%c#%c#%c#  \n" (if sa >= 3 then tpt_to_letter (List.nth la (sa-3)) else ' ')
   (if sb >= 3 then tpt_to_letter (List.nth lb (sb-3)) else ' ')
   (if sc >= 3 then tpt_to_letter (List.nth lc (sc-3)) else ' ')
   (if sd >= 3 then tpt_to_letter (List.nth ld (sd-3)) else ' ') ;

   Printf.printf "  #%c#%c#%c#%c#  \n" (if sa >= 2 then tpt_to_letter (List.nth la (sa-2)) else ' ')
    (if sb >= 2 then tpt_to_letter (List.nth lb (sb-2)) else ' ')
    (if sc >= 2 then tpt_to_letter (List.nth lc (sc-2)) else ' ')
    (if sd >= 2 then tpt_to_letter (List.nth ld (sd-2)) else ' ') ;

  Printf.printf "  #%c#%c#%c#%c#  \n" (if sa >= 1 then tpt_to_letter (List.nth la (sa-1)) else ' ')
   (if sb >= 1 then tpt_to_letter (List.nth lb (sb-1)) else ' ')
   (if sc >= 1 then tpt_to_letter (List.nth lc (sc-1)) else ' ')
   (if sd >= 1 then tpt_to_letter (List.nth ld (sd-1)) else ' ') ;
   Printf.printf "  #########  \n";
;;


let compare_puzzle a0 (la0, lb0, lc0, ld0) a1 (la1, lb1, lc1, ld1) =
  let rec nest = function
  | ([], []) -> true
  | ([], _) | (_, []) -> false
  | (x::lx, y::ly) -> if x == y then nest (lx, ly) else false
  in
    (*Check array position*)
  let acmp = Array.init 11 (fun i -> a0.(i) == a1.(i)) in
  if (Array.fold_left (&&) true acmp)
  then if nest (la0, la1)
    then if nest (lb0, lb1)
      then if nest (lc0, lc1)
        then nest (ld0, ld1)
        else false
      else false
    else false
  else false
;;


let solve_puzzle puzzle =
  (*mem: puzzle solved*)
  let rec nest k vmem = function
  | [] -> vmem
  | (arr, pzl, score)::lst_puzzle -> (

    (*Need to generate all possibilities*)
    if k mod 1000 == 0
    then (Printf.printf "Puzzle size: %d.\t Solutions: %d" (List.length lst_puzzle)  vmem;
      print_newline ();
    );

    let (tfa, tfb, tfc, tfd) = check_columns pzl in
    if is_finished (tfa, tfb, tfc, tfd) arr
    then (
      (*
      (*Reduce the number of puzzle to check*)
      Printf.printf "\tFinished with %d vs %d\n" score vmem;
      *)
      let vmin = min score vmem in
      let lst_new_puzzle = List.fold_left (fun mem (ai, pi, si)->
        if si >= vmin then mem
        else (ai, pi, si)::mem
        ) [] lst_puzzle in
      nest (k+1) vmin lst_puzzle
      )
    else (

      let steps_a = if tfa then  move_in arr Amber else move_out arr Amber in
      let laa = List.map (update_input arr pzl score Amber) steps_a in

      let steps_b = if tfb then move_in arr Bronze else move_out arr Bronze in
      let lbb = List.map (update_input arr pzl score Bronze) steps_b in

      let steps_c = if tfc then move_in arr Copper else move_out arr Copper in
      let lcc = List.map (update_input arr pzl score Copper) steps_c in

      let steps_d = if tfd then move_in arr Desert else move_out arr Desert in

      let ldd = List.map (update_input arr pzl score Desert) steps_d in

      let l_all = laa @ lbb @ lcc @ ldd in
      (*
      let s_all = List.length l_all in
      if s_all == 0
      then (
        print_arr arr;
        print_full arr pzl;
        Printf.printf "\tDead end\n"
      )
      else Printf.printf "Found %d New possibilities. Left: %d \n" (s_all) (List.length lst_puzzle);

      *)
      nest (k+1) vmem (l_all @ lst_puzzle)
    )
  ) in nest 0 10000000000000 [Array.make 11 Empty, puzzle, 0]
;;

let my_puzzle = ([Copper; Desert; Desert; Bronze],
  [Desert; Copper; Bronze; Amber],
  [Amber; Bronze; Amber; Desert],
  [Bronze; Amber; Copper; Copper]);;


let solution = solve_puzzle my_puzzle;;

(*
Our code does not take into account the cost of going out
2 * (1 + 2 + 3 + 4) = 20 (to move in and out all stuff)
= 22220
*)

Printf.printf "Solution: %d + %d = %d\n" solution 22220 (solution + 22220);;
