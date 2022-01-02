type fish =
  | Node of fish * fish
  | Bottom of int
;;

let identify_parts stri =
  let rec nest mem i v =
  if mem == 0
  then (i, v)
  else if stri.[i] == '[' then nest (mem+1) (i+1) v
  else if stri.[i] == ']' then nest (mem-1) (i+1) v
  else if (stri.[i] == ',') && (mem == 1)  then nest mem (i+1) i
  else nest mem (i+1) v
  in nest 1 1 (-1) (*Last field for the virgule*)
;;

let parse_line stri =
  let rec nest word =

  if word.[0] == '['
  then (
    let (imax, v) = identify_parts word in
    let part1 = String.sub word 1 (v-1)
    and part2 = String.sub word (v+1) (imax-v-2) (*-1 to take into account the avoidance of ], -1 for the comma shift*)
    in
    Node (nest part1, nest part2)
    )
  else Bottom (int_of_char word.[0] - 48)
  in nest stri
;;


let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    nest ((parse_line line)::lst)
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    (List.rev lst)
  in nest []
;;


let print_fish_line tree =
  let rec nest = function
  | Bottom x -> Printf.printf "%d" x
  | Node (x, y) -> (
    Printf.printf "[";
    nest x;
    Printf.printf ",";
    nest y;
    Printf.printf "]";
    ) in nest tree;
    print_newline ()
;;


let measure_max_depth snailfish =
  let rec nest = function
  | Bottom x -> 1
  | Node (x, y) -> 1+ max (nest x) (nest y)
  in nest snailfish
;;


let rec split_val_rec x =
  if x > 9
  then
    let x1 = x / 2 in
    let x2 = x - x1 in
    Node (split_val_rec x1, split_val_rec x2)
  else Bottom x
;;

let rec split_val x =
  if x > 9
  then
    let x1 = x / 2 in
    let x2 = x - x1 in
    (true, Node (Bottom x1, Bottom x2))
  else (false, Bottom x)
;;

let add_to_right tree v =
  let rec nest = function
  | Bottom x -> Bottom (x+v)
  | Node (x, y) -> Node (nest x, y)
  in nest tree
;;

let add_to_left tree v =
  let rec nest = function
  | Bottom x -> Bottom (x+v)
  | Node (x, y) -> Node (x, nest y)
  in nest tree
;;

let rec reduce_completely tree =
  match tree with
  | Bottom x -> (false, Bottom x, 0, 0) (*No operation done*)
  | Node (Bottom x, Bottom y) -> (
    (true, Bottom 0, x, y)
    )
  | _ -> (
    Printf.printf "ERROR !\n";
    (true, Bottom 0, 0, 0)
    )
;;

let explode_unit tree =
  let rec nest d tr =
  if d < 4
  then match tr with
  | Node (x, y) -> (
    let (tf0, x1, l1, r1) = nest (d+1) x in
    if tf0 then (
      let y1 = add_to_right y r1 in
      (true, Node (x1, y1), l1, 0)
      )
    else
      let (tf1, y1, l2, r2) = nest (d+1) y in
      if tf1 then (
        let x1 = add_to_left x l2 in
        (true, Node (x1, y1), 0, r2)
        )
      else (false, Node (x, y), 0, 0)
      )
  | Bottom x -> (false, Bottom x, 0, 0)
  else reduce_completely tr
in nest 0 tree
;;

let simplify tree =
  let rec nest = function
  | Bottom x -> (
    let (tf, x1) = split_val x in
    (tf, x1)
    )
  | Node (x, y) -> (
    let (tf0, x1) = nest x in
    if tf0 then (tf0, Node (x1, y))
    else (
      let (tf1, y1) = nest y in
      (tf1, Node (x1, y1))
      )
    ) in nest tree
;;


let reduce_depth tree =
  let rec nest tr c =
  if c == 0
  then (
    let (tf, tr0, _, _) = explode_unit tr in
    if tf then nest tr0 0
    else nest tr0 1
    )
  else (
    let (tf, tr0) = simplify tr in
    if tf then nest tr0 0
    else tr0
    )
  in nest tree 0
;;

let reduce_fishes lst =
  let l1 = List.map (reduce_depth) lst in

  let rec nest = function
  | x::[] -> x
  | x::y::lx -> nest ((reduce_depth (Node (x, y)))::lx)
  in nest l1
;;

let get_magnitude tree =

  let rec nest = function
  | Node (x, y) -> 3 * (nest x) + 2 * (nest y)
  | Bottom x -> x
  in nest tree
;;

(*Part 2*)

let best_magnitude lst =
  let l1 = List.map (reduce_depth) lst in
  let res = List.mapi (fun i x ->
    List.mapi (fun j y ->
      if i == j then 0
      else get_magnitude (reduce_depth (Node (x, y)))
      ) l1
    ) l1 in
  List.fold_left (fun mem row ->
    List.fold_left (max) mem row
    ) 0 res
;;



let snail_lst = read_file "input_18.txt";;
let ttt = reduce_fishes snail_lst;;
get_magnitude ttt;;
print_fish_line ttt;;

let snail_lst = read_file "input_18.txt";;
best_magnitude snail_lst;;
