let read_input_data path ly =
  let ic = open_in path in
  let mat = Array.init ly (fun _ ->
    let line = input_line ic in
    let l = String.length line in
    Array.init l (fun i ->
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


let data = read_input_data "input_24" 5;;
let data_test = read_input_data "input_24_test" 5;;

let tile_to_int mat i j =
  if mat.(i).(j) == '.'
  then 0 else 1
;;


let count_tiles mat i j =
  let lst = (
    if (i == 0) then [(1, 0)]
    else if (i==4) then [(-1, 0)]
    else [(1, 0); (-1, 0)]
    ) @ (
    if (j == 0) then [(0, 1)]
    else if (j==4) then [(0, -1)]
    else [(0, 1); (0, -1)]) in

  List.fold_left (fun mem (i1, j1) ->
          mem + (tile_to_int mat (i+i1) (j+j1))
        ) 0 lst
;;




let update mat =
  let lx = Array.length mat.(0)
  and ly = Array.length mat in

  Array.init ly (fun i ->
    Array.init lx (fun j ->
      let c = count_tiles mat i j in
      if (mat.(i).(j) == '.') && ((c == 1) || (c==2))
      then '#'
      else if (mat.(i).(j) == '#') && (c!=1)
      then '.'
      else mat.(i).(j)
      )
    )
  ;;


let k_update mat k =
  let rec nest mati ki =
  if k == ki
  then mati
  else (
    Printf.printf "\nStep: %d" ki;
    print_newline ();
    print_puzzle mati;
    nest (update mati) (ki+1))
  in nest mat 0
;;

let make_bio_score i0 j0 =
  let arr = Array.init i0 (fun _ -> Array.make j0 0) in
  Array.iteri (fun i row ->
    if i == 0
    then Array.iteri (fun j _ ->
      if j == 0
      then arr.(0).(0) <- 1
      else arr.(0).(j) <- arr.(0).(j-1) * 2
      ) row
    else Array.iteri (fun j _ ->
      arr.(i).(j) <- arr.(i-1).(j) * 2 * arr.(0).(j0-1)
      ) row
    ) arr;
  arr
;;

let scoring = make_bio_score 5 5;;
let get_score mat =
  let sci = Array.mapi (fun i row ->
    Array.mapi (fun j x ->
      if x == '#'
      then scoring.(i).(j)
      else 0
      ) row
    ) mat in

  Array.fold_left (fun mem row ->
    mem + Array.fold_left (+) 0 row
    ) 0 sci
;;

let solve_part_1 mat =
  let hsh = Hashtbl.create 100 in

  let rec nest mati i =
    let s = get_score mati in
    Printf.printf "%d: \t%d\n" i s;
    match Hashtbl.find_opt hsh s with
    | None -> (
      Hashtbl.add hsh s i;
      nest (update mati) (i+1))
    | x -> (x, i, s)
  in nest mat 0
;;



(*
[|
[|'.'; '#'; '.'; '#'; '.'|];
[|'.'; '#'; '#'; '.'; '.'|];
[|'.'; '#'; '.'; '.'; '.'|];
[|'.'; '#'; '#'; '#'; '.'|];
[|'#'; '#'; '.'; '.'; '#'|]|]

    *)

(*PART 2*)

let count_border tile i j di dj =
  let xx = Array.mapi (fun k _ ->
    tile_to_int tile (i+ di*k) (j + dj*k)
    ) tile  in
  Array.fold_left (+) 0 xx
;;




let count_tiles_v2 arr_grid lvl i j =
  (*Left border*)
  let c0 = if (j == 0)
  then  tile_to_int arr_grid.(lvl-1) 2 1
  else if (i == 2) && (j==3)
  then count_border arr_grid.(lvl+1) 0 4 1 0
  else tile_to_int arr_grid.(lvl) (i) (j-1) in

  (*Righ border *)
  let c1 = if (j == 4)
  then  tile_to_int arr_grid.(lvl-1) 2 3
  else if (i == 2) && (j==1)
  then count_border arr_grid.(lvl+1) 0 0 1 0
  else tile_to_int arr_grid.(lvl) (i) (j+1) in

  (*Top border*)
  let c2 = if (i == 0)
  then  tile_to_int arr_grid.(lvl-1) 1 2
  else if (i == 3) && (j == 2)
  then count_border arr_grid.(lvl+1) 4 0 0 1
  else tile_to_int arr_grid.(lvl) (i-1) (j) in

  (*Bottom*)
  let c3 = if (i == 4)
  then  tile_to_int arr_grid.(lvl-1) 3 2
  else if (i == 1) && (j == 2)
  then count_border arr_grid.(lvl+1) 0 0 0 1
  else tile_to_int arr_grid.(lvl) (i+1) (j) in

  c0 + c1 + c2 + c3
;;


let update_lvl lvl arr_grid =
  Array.init 5 (fun i ->
    Array.init 5 (fun j ->
      if (i == 2) && (j==2) then '.'
      else (
        let c = count_tiles_v2 arr_grid lvl i j in

        if (arr_grid.(lvl).(i).(j) == '.') && ((c == 1) || (c==2))
        then '#'
        else if (arr_grid.(lvl).(i).(j) == '#') && (c!=1)
        then '.'
        else arr_grid.(lvl).(i).(j)

        )
      )
    )
;;

let update_v2 arr_grid =
  let l = Array.length arr_grid in
  Array.mapi (fun i x ->
    if (i == 0) || (i== l-1)
    then x
    else update_lvl i arr_grid
    ) arr_grid;;

let solve_2 k tile =
  let arr_grid = Array.init (2*k+1) (fun i ->
    if i == k
    then Array.map (fun row -> Array.copy row) tile
    else Array.init 5 (fun i -> Array.make 5 '.')
    ) in

  let rec nest arr_gi i =
  if i == k
  then arr_gi
  else (
    nest (update_v2 arr_gi) (i+1)
    ) in nest arr_grid 0
;;

let print_level arr_grid =
  let k = (Array.length arr_grid) /2 in
  Array.iteri (fun i tile ->
    Printf.printf "\nLEVEL %d\n" (i-k);
    print_puzzle tile
    ) arr_grid
;;

let count_all arr_grid = 
  Array.fold_left (fun mem tile ->
    Array.fold_left (fun m1 row ->
      Array.fold_left (fun m2 x ->
        if x == '.' then m2 else m2+1
        ) m1 row
      ) mem tile
    ) 0 arr_grid
;;
