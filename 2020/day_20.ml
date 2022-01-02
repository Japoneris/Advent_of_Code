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

let data = read_file "input_20";;
let data_t = read_file "input_20_t";;

let the_puzzles = parse_full_puzzl data;;
let the_puzzles_t = parse_full_puzzl data_t;;


type tile =
| Empty
| Full
;;

type puzzle = {idx: int; p: tile array array};;

let parse_number line =
  Scanf.sscanf line "Tile %d:" (fun x -> x);;


let parse_line line =
  let lmax = String.length line in
  Array.init lmax (fun i ->
    if line.[i] == '#'
    then Full
    else Empty
    )
;;

let parse_puzzle lines =
  let rec nest mem = function
  | [] -> (Array.of_list (List.rev mem), [])
  | x::lx ->
    if String.length x == 0
    then (Array.of_list (List.rev mem), lx)
    else nest ((parse_line x)::mem) lx
  in
  let (l1::l2) = lines in
  let (puzzle, left) = nest [] l2
  and idx = parse_number l1
  in ({idx=idx; p=puzzle}, left)
;;

let parse_full_puzzl data =
  let rec nest mem = function
  | [] -> mem
  | lx -> (
    let (puzzle, left) = parse_puzzle lx
    in nest (puzzle::mem) left
    )
  in nest [] data
;;

type rotation =
  | Front
  | Left
  | Right
  | Back
;;

let rotnext = function
  | Front -> Left
  | Left -> Back
  | Back -> Right
  | Right -> Front
;;

let print_puzzle p =
  Array.iter (fun row ->
    Array.iter (fun c ->
      Printf.printf "%c" (match c with
      | Empty -> '.'
      | Full -> '#')
      ) row;
    print_newline()
    ) p
;;

(*Rotate the raw puzzle*)
let rotate p =
  let l = Array.length p
  and l1 = Array.length p.(0) in
  Array.init l1 (fun i ->
    Array.init l (fun j ->
      p.(j).(l1-i-1)
      )
    )
;;

let flip p =
  let l = Array.length p
  and l1 = Array.length p.(0) in

  Array.init l (fun i ->
    Array.init l1 (fun j ->
      p.(l-i-1).(j)
      )
    )
;;

(*Check if 2 tiles match exactly in their position*)
let matching p0 p1 =
  let l = Array.length p0
  and l1 = Array.length p0.(0) in
  let rec nest i =
  if i == l
  then true
  else match (p0.(i).(l1-1), p1.(i).(0)) with
  | (Empty, Empty) -> nest (i+1)
  | (Full, Full) -> nest (i+1)
  | _ -> false
  in nest 0
;;

let gen_rotation p =
  let p1 = rotate p in
  let p2 = rotate p1 in
  let p3 = rotate p2 in
  p::p1::p2::p3::[]
;;

let gen_rotation_1 p =
  let ll = gen_rotation p in
  ll@(List.map (flip) ll)
;;


let check_match pz0 pz1 =
  let p0 = gen_rotation pz0
  and p1 = gen_rotation_1 pz1 in

  let rec nest = function
  | ([], _) -> false
  | (x::lx, []) -> nest (lx, p1)
  | (x::lx, y::ly) -> (
    if matching x y
    then true
    else nest (x::lx, ly)
    ) in nest (p0, p1)
;;

let check_matching_puzzle p lst_p =
  List.fold_left (fun mem pzi ->
    if pzi.idx == p.idx
    then mem
    else if check_match p.p pzi.p
    then pzi.idx::mem
    else mem
    ) [] lst_p
;;


let positions = List.fold_left (fun mem pz ->
  let r = (check_matching_puzzle pz the_puzzles) in
  if List.length r == 2
  then (
    Printf.printf "%d: %d\n" pz.idx (List.length r);
    );
  (pz.idx, r)::mem
  ) [] the_puzzles;;

let positions_t = List.fold_left (fun mem pz ->
  let r = (check_matching_puzzle pz the_puzzles_t) in
  Printf.printf "%d: %d\n" pz.idx (List.length r);
  (pz.idx, r)::mem
  ) [] the_puzzles_t;;


type puzzle_pos =
| Unknown
| Id of int
;;

let recover_constraints mat i0 j0 =
  let l = Array.length mat in

  let rec nest mem c = function
  | [] -> (mem, c)
  | (di, dj)::lx -> (
    if (i0 + di) < 0 || (i0 + di) >= l || (j0 + dj < 0) || (j0 + dj >= l)
    then nest mem c lx
    else match mat.(i0+di).(j0+dj) with
    | Unknown -> nest mem (c+1) lx
    | Id v -> nest (v::mem) (c+1) lx
    ) in nest [] 0 [(-1, 0); (1, 0); (0, -1); (0, 1)]
;;

let verify_lst lst lst_1 =
  let rec nest = function
  | [] -> true
  | x::lx -> (
    if (List.fold_left (fun mem y ->
      mem || (x==y)
      ) false lst)
    then nest lx else false
    ) in nest lst_1
;;


let reconstruct_position data l =
  let the_puzzle = parse_full_puzzl data in
  let positions = List.fold_left (fun mem pz ->
    let r = (check_matching_puzzle pz the_puzzles) in
    (pz.idx, r)::mem
    ) [] the_puzzles in

  (*corner*)
  let (c0::_) = List.fold_left (fun mem (idx, l_idx) ->
    if List.length l_idx == 2 then idx::mem else mem
  ) []  positions in


  let mat = Array.init l (fun _ -> Array.init l (fun i -> Unknown)) in
  mat.(0).(0) <- Id c0;

  let rec nest i j left =
  if i == l then nest 0 (j+1) left
  else if j == l then mat (*End*)
  else (
    let (lst_c, nc) = recover_constraints mat i j in
    let results = List.fold_left (fun mem (idx, lst_id) ->
      if List.length lst_id != nc
      then mem
      else if verify_lst lst_id lst_c
      then idx::mem
      else mem
    ) [] left in

    match results with
    | [] -> (Printf.printf "Error at (%d, %d)" i j; mat)
    | x::lx -> (
      mat.(i).(j) <- Id x;
      let new_left = List.fold_left (fun mem (idx,lst_idx) ->
        if idx == x then mem else (idx, lst_idx)::mem
      ) [] left in
      nest (i+1) j new_left
      )
    ) in nest 1 0 positions
;;



let full_puzzle = reconstruct_position data l;;
let the_puzzle = parse_full_puzzl data;;



let extract_ordered pz0 pz1 =
  let p0 = gen_rotation pz0
  and p1 = gen_rotation_1 pz1 in

  let rec nest = function
  | (x::lx, []) -> nest (lx, p1)
  | (x::lx, y::ly) -> (
    if (Array.length x.(0)) == (Array.length y.(0))
    then (
      if matching x y
      then (x, y)
      else nest (x::lx, ly)
      )
    else nest (x::lx, ly)
    ) in nest (p0, p1)
;;


let extract_ordered_v2 pz0 pz1 l0 =
  let p0 = gen_rotation pz0
  and p1 = gen_rotation_1 pz1 in

  let rec nest = function
  | (x::lx, []) -> nest (lx, p1)
  | (x::lx, y::ly) -> (
    if ((Array.length x) == l0) && (Array.length y == l0)
    then (
      if matching x y
      then (x, y)
      else nest (x::lx, ly)
      )
    else nest (x::lx, ly)
    ) in nest (p0, p1)
;;


let join_puzzle_v2 lst_puzzle lii =
  let rec nest =  function
  | ([], x::y::lx) -> (*Firt position to find*) (
    let (pzx, pzy) = extract_ordered_v2 x y lii in
    nest (pzx::[], pzy::lx)
    )
  | (x::mem, y::lx) -> (
    let (pzx, pzy) = extract_ordered_v2 x y lii in
    nest (pzy::pzx::mem, lx)
    )
  | (mem, []) -> List.rev mem
  in
  let lst1 = nest ([], lst_puzzle) in
  (*Create a single matrix*)
  let l = Array.length (List.hd lst1) in
  let l1 = Array.length (List.hd lst1).(0) in
  let l2 = List.length lst1 in

  (*True version *)
  let mat = Array.init l  (fun _ -> Array.make (l2*(l1-2)) Empty) in
  List.iteri (fun p m0 ->
    Printf.printf "Block %d\n" p;
    Array.iteri (fun i row ->
      Array.iteri (fun j x ->
        if (j == 0) || (j == l1-1)
        then ()
        else (
          Printf.printf "%d %d=> %d \n" i j (p*(l1-2) + j-1);
          mat.(i).(p*(l1-2) + j-1) <- x
          )
        ) row
      ) m0
    ) lst1;
  mat
  ;;




let join_puzzle lst_puzzle =
  let rec nest =  function
  | ([], x::y::lx) -> (*Firt position to find*) (
    let (pzx, pzy) = extract_ordered x y in
    nest (pzx::[], pzy::lx)
    )
  | (x::mem, y::lx) -> (
    let (pzx, pzy) = extract_ordered x y in
    nest (pzy::pzx::mem, lx)
    )
  | (mem, []) -> List.rev mem
  in
  let lst1 = nest ([], lst_puzzle) in

  (*Create a single matrix*)
  let l = Array.length (List.hd lst1) in
  let l1 = Array.length (List.hd lst1).(0) in
  let l2 = List.length lst1 in

  (*True version *)
  let mat = Array.init l  (fun _ -> Array.make (l2*(l1-2)) Empty) in
  List.iteri (fun p m0 ->
    Printf.printf "Block %d\n" p;
    Array.iteri (fun i row ->
      Array.iteri (fun j x ->
        if (j == 0) || (j == l1-1)
        then ()
        else (
          Printf.printf "%d %d=> %d \n" i j (p*(l1-2) + j-1);
          mat.(i).(p*(l1-2) + j-1) <- x
          )
        ) row
      ) m0
    ) lst1;
  mat
  ;;

  (*Debug version
  let mat = Array.init (l2*l1) (fun _ -> Array.make l Empty) in
  List.iteri (fun p m0 ->
    Array.iteri (fun i row ->
      Array.iteri (fun j x ->
        mat.(i).(p*l1 + j) <- x
        ) row
      ) m0
    ) lst1;
  mat
  *)

let merge_a_line line_id all_puzzles =
  (*Recover puzzle with good id*)
  let a1 = List.map (fun (Id idx) ->
    List.fold_left (fun mem pz ->
      if pz.idx == idx then pz.p else mem
    ) [|[|Empty|]|] all_puzzles
    ) (Array.to_list line_id) in

  join_puzzle a1
;;




let merge_image line_order puzzles =
      (Array.to_list (
      Array.map (fun line ->
        let a1 = merge_a_line line puzzles in
        Printf.printf "%d\n" (Array.length a1);
        rotate a1
        )
      line_order
      )
    )
;;


let image = merge_image full_puzzle the_puzzles;;
print_puzzle image;;

let sea_string = [|"                  # ";
"#    ##    ##    ###";
" #  #  #  #  #  #   "|];;
let sea_monster = Array.map (parse_line) sea_string;;

let puzzle_exp = Array.of_list (List.map (parse_line) (read_file "data_20_t2"));;


let monster_match image krak i0 j0 =
  let ay = Array.mapi (fun i row ->
    let ax = Array.mapi (fun j x ->
      match  (x, image.(i0+i).(j0+j)) with
      | (Empty, _) -> true
      | (Full, Full) -> true
      | (Full, Empty) -> false
      ) row in
    Array.fold_left (fun x y -> x && y) true ax
    ) krak in
  Array.fold_left (fun x y -> x && y) true ay
;;



let match_kraken image krak =
  let l0 = Array.length image
  and l1 = Array.length image.(0)
  and k0 = Array.length krak
  and k1 = Array.length krak.(0) in

  let rec nest i j cnt =
  if (i > l0-k0)
  then nest 0 (j+1) cnt
  else if (j > l1 - k1)
  then cnt
  else if monster_match image krak i j
  then (
    Printf.printf "%d %d\n" i j;

    nest (i+1) j (cnt +1))
  else nest (i+1) j cnt
  in nest 0 0 0
;;


let count_tiles image =
  Array.fold_left (fun m0 row ->
    Array.fold_left (fun mem x ->
      match x with
      | Empty -> mem
      | Full -> mem+1
      ) m0 row
    ) 0 image
  ;;
