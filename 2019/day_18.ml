
let read_input_data path =
  let ic = open_in path in
  let mat = Array.init 81 (fun _ ->
    let line = input_line ic in
    Array.init 81 (fun i ->
      line.[i]
      )
    ) in
  close_in_noerr ic;
  mat
;;

let print_puzzle mat =
  Array.iter (fun row ->
    Array.iter (Printf.printf "%c") row;
    print_newline ()
  ) mat
;;



type keydoo = Door of char | Key of char | Start | Block | Way;;
type position   = {x:int; y:int};;
type position_d = {x:int; y:int; d:int};;
type objet = {p: position_d; o: keydoo};;

let process c =
  let ic = int_of_char c in
  if 'c' == '@'
  then Start
  else if c == '#'
  then Block
  else if (ic <= 90 ) && (ic >= 65)
  then Door (char_of_int (ic+32))
  else if (ic <=122) && (ic >= 97)
  then Key (char_of_int ic)
  else Way
;;



let get_next_position pt =
  List.map (fun (a,b) -> {x=pt.x+a; y=pt.y+b}) [(0, 1), (0, -1), (1, 0), (-1, 0)]
;;

let get_next_position_d pt =
  List.map (fun (a,b) -> {x=pt.x+a; y=pt.y+b; d=pt.d+1}) [(0, 1); (0, -1); (1, 0); (-1, 0)]
;;


let move_puzzle puzzle p0 =
  let mat = Array.copy puzzle in
  let fx = (get_next_position_d) in

  let rec nest mem = function
  | [] -> mem
  | pt::lst -> match process mat.(pt.y).(pt.x) with
  | Door c -> (
    mat.(pt.y).(pt.x) <- '#';
    nest ({o=Door c; p=pt}::mem) ((fx pt)@lst)
    )
  | Key c -> (
    mat.(pt.y).(pt.x) <- '#';
    nest ({o=Key c; p=pt}::mem) ((fx pt)@lst)
    )
  | Way | Start -> (
    mat.(pt.y).(pt.x) <- '#';
    nest mem ((fx pt)@lst)
    )
  | Block -> nest mem lst
  in nest [] [p0]
;;


let puzzle = read_input_data "input_18";;
let all_items = move_puzzle puzzle {x=40; y=40; d=0};;

let item_to_id = function
| Key c  -> (int_of_char c) - 97
| Door c -> (int_of_char c) + 26 - 97
| Start -> 26*2
| _ -> -1
;;

let make_distance_matrix path =
  let dmax = 10000000000 in
  let dist = Array.init (26*2+1) (fun _ -> Array.make (26*2+1) dmax) in

  let rec nest = function
  | [] -> dist
  | obj::lst -> (
    let i = item_to_id obj.o in
    dist.(i).(26*2) <- obj.p.d;
    dist.(26*2).(i) <- obj.p.d;

    let puzzle = read_input_data path in
    let items  = move_puzzle puzzle {x=obj.p.x; y=obj.p.y; d=0} in
    List.iter (fun oo ->
      let j = item_to_id oo.o in
      dist.(i).(j) <- oo.p.d
      ) items;
    nest lst
    ) in

  let puzzle = read_input_data path in
  nest (move_puzzle puzzle {x=40; y=40; d=0})
;;
