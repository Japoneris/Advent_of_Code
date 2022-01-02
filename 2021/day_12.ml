
type cave_type =
  | Start
  | End
  | Large of string
  | Small of string
;;

let is_small word =
  if String.equal word "start" then Start
  else if String.equal word "end" then End
  else if String.equal word (String.uncapitalize_ascii word)
  then Small word
  else Large word
;;

let parse_line line =
  let (a::b::[]) = String.split_on_char '-' line in
  (is_small b, is_small a)
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


let is_equal cave0 cave1 =
  match (cave0, cave1) with
  | (Start, Start) | (End, End) -> true
  | (Start, _) | (End, _) | (_, Start) | (_, End) -> false
  | (Large x, Small y) | (Small x, Large y) -> false
  | (Small x, Small y) -> String.equal x y
  | (Large x, Large y) -> String.equal x y
;;

let compare cave0 cave1 =
  match (cave0, cave1) with
  | (Start, Start) | (End, End) -> 0
  | (Start, _) | (_, End) -> +1
  | (_, Start) | (End, _) -> -1
  | (Large x, Small y) -> +1
  | (Small x, Large y) -> -1
  | (Small x, Small y) -> String.compare x y
  | (Large x, Large y) -> String.compare x y
;;

let get_unique_position gpx =
  let lst = List.fold_left (fun mem (a, b) -> a::b::mem) [] gpx in
  let lst1 = List.sort (compare) lst in

  let rec nest mem = function
  | [] -> mem
  | x::[] -> x::mem
  | x::y::lst -> if is_equal x y
    then nest mem (x::lst)
    else nest (x::mem) (y::lst)
    in nest [] lst1
;;


let graph = read_file "input_12.txt";;
let position = get_unique_position graph;;

(*Not finished ...*)
