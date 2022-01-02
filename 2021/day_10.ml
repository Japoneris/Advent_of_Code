type symbol =
 | Open of char
 | Close of char
;;


let parse_line line =
  let l0 = String.length line in
  List.init l0 (fun i ->
    let ci = line.[i] in
    if (ci == '[') || (ci == '<') || (ci == '(') || (ci == '{')
    then Open ci
    else Close ci
    )
;;

let read_file file =
  let ic = open_in file in
  let rec nest lst =
    try
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    nest ((parse_line line)::lst)
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    Array.of_list (List.rev lst)
  in nest []
  ;;


let is_okay c0 c1 =
  if c0 == '{'  then c1 == '}'
  else if c0 == '(' then c1 == ')'
  else if c0 == '<' then c1 == '>'
  else if c0 == '[' then c1 == ']'
  else if c0 == ' ' then c1 == ' '
  else false
;;

let is_valid line =

  let rec nest cmem = function
  | [] -> (true, [])
  | (Open c)::lst -> (
    let (tf, lsti) = nest c lst in
    if tf
    then nest cmem lsti (*All intermediate parenthesis are closed*)
    else (false, lsti)
    )
  | (Close c)::lst ->
    if is_okay cmem c
    then (true, lst)
    else (false, (Close c)::lst)
  in nest ' ' line
;;

let arr = Array.map (fun line ->
  let (tf, lsx) = is_valid line in
  if tf then 0
  else match List.hd lsx with
  | Close a -> if a == ')' then 3
    else if a == ']' then 57
    else if a == '}' then 1197
    else if a == '>' then 25137
    else 0
  | Open a -> 0
  ) lst
;;
let res = Array.fold_left (+) 0 arr;;


(*Need to forget about closed characters*)
let is_finished_v2 line =
  let rec nest = function
  | ([], lsy) -> (true, lsy)
  | ((Open x)::lsx, lsy) -> nest (lsx, (Open x)::lsy)
  | (Close x)::lsx, (Open y)::lsy ->
    if is_okay y x
    then nest (lsx, lsy)
    else (false, []) (*END point*)
  in nest (line, [])
;;

let get_score subline =
  let rec nest vi = function
  | [] -> vi
  | (Open c)::lsx ->
    nest (vi * 5 + (if c == '(' then 1
    else if c == '[' then 2
    else if c == '{' then 3
    else 4)
    ) lsx
  in nest 0 subline
;;


let lst = read_file "input_10.txt";;

let lst1 = Array.fold_left (fun mem line ->
  let (tf, subline) = is_finished_v2 line in
  if tf
  then (get_score subline)::mem else mem
  ) [] lst ;;
let lst2 = List.sort (-) lst1;;

let n0 = List.length lst2;;
