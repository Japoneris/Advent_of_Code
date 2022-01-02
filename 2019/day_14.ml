
type chemical = {r: string; q:int};;
type chemical_reaction = {rea: chemical list; prod: chemical};;

let scanint x = Scanf.sscanf x "%d %s " (fun y z -> {r=z; q=y});;
let scanint_2 x = Scanf.sscanf x " %d %s " (fun y z -> {r=z; q=y});;

let read_puzzle path =
    let ic = open_in path in

    let rec nest lst =
    try
      nest ((input_line ic)::lst)
     with e -> close_in_noerr ic;
     lst in
    nest []
  ;;

let get_reactant line =
  let l1 = String.split_on_char ',' line in
  List.map (scanint) l1
;;

let transform_line line =
  let p1::p2::[] = String.split_on_char '=' line in
  let p3 = String.sub p2 2 (String.length p2 - 2) in
  {rea = List.map (scanint_2) (String.split_on_char ',' p1);
  prod = scanint_2 p3}
;;



let make_hashtbl rea =
  let tbl = Hashtbl.create 100 in

  List.iteri (fun i x ->
    Hashtbl.add tbl x.prod.r (i, x)
    ) rea;
  tbl
;;

let factor x q =
  if (q / x) * x == q
  then (q/x)
  else (q/x) + 1
;;

let search tbl pat0 pat1 =
  (*param c: number of item to be produced*)

  let arr = Array.make (Hashtbl.length tbl) 0 in

  let rec nest pat c =
  if String.compare pat pat0 == 0
  then c
  else (
    Printf.printf "%s\n" pat;
    let (i, r_1) = Hashtbl.find tbl pat in
    let cneed = c - arr.(i) in (*What is asked - whzt we already have*)
    if c < arr.(i)
    then (
      arr.(i) <- arr.(i) - c;
      0
      )
    else (
      let cprod = r_1.prod.q in (*What we can produce by one step of reaction*)
      let v1 = (List.fold_left (fun mem x -> mem + (nest x.r x.q)) 0 r_1.rea) in

      let f = factor cprod cneed in
      Printf.printf "Produced %d x %d unit of %s (%d in stock) for %d required\n" f cprod pat (arr.(i)) c;
      arr.(i) <- arr.(i) + f * cprod - c;
      Printf.printf "%d ORE needed for %d %s\n" v1 cprod pat ;
      v1 * f
      )
    )
  in nest pat1 1
;;

let search tbl pat0 pat1 =
  let arr = Array.make (Hashtbl.length tbl) 0 in

  (*param c: number of item to be produced*)
  let rec nest pat c =
  if String.compare pat pat0 == 0
  then c
  else (
    Printf.printf "%s\n" pat;
    let (i, r_1) = Hashtbl.find tbl pat in
    let cneed = c - arr.(i) in (*What is asked - whzt we already have*)

    if c < arr.(i)
    then (  arr.(i) <- arr.(i) - c; 0)
    else (
      let cprod = r_1.prod.q in (*What we can produce by one step of reaction*)
      let f = factor cprod cneed in (*Number of times the reaction needs to be done*)

      let v1 = (List.fold_left (fun mem x -> mem + (nest x.r (f*x.q))) 0 r_1.rea) in

      Printf.printf "Produced %d x %d unit of %s (%d in stock) for %d required\n" f cprod pat (arr.(i)) c;
      arr.(i) <- arr.(i) + f * cprod - c;
      Printf.printf "%d ORE needed for %d %s\n" v1 cprod pat ;
      v1
      )
    )
  in nest pat1 1
;;



let puzzle = read_puzzle "input_14";;
let reactions = List.map (transform_line) puzzle;;
let tbl = make_hashtbl reactions;;
search tbl "ORE" "FUEL";;


let input_test = "157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT";;

let input_test = "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF";;

let puzzle_test = String.split_on_char '\n' input_test ;;
let reactions = List.map (transform_line) puzzle_test;;
let tbl = make_hashtbl reactions;;
search tbl "ORE" "FUEL";;


let search_k tbl pat0 pat1 k =
  let arr = Array.make (Hashtbl.length tbl) 0 in

  (*param c: number of item to be produced*)
  let rec nest pat c =
  if String.compare pat pat0 == 0
  then c
  else (
    let (i, r_1) = Hashtbl.find tbl pat in
    let cneed = c - arr.(i) in (*What is asked - whzt we already have*)

    if c < arr.(i)
    then (  arr.(i) <- arr.(i) - c; 0)
    else (
      let cprod = r_1.prod.q in (*What we can produce by one step of reaction*)
      let f = factor cprod cneed in (*Number of times the reaction needs to be done*)

      let v1 = (List.fold_left (fun mem x -> mem + (nest x.r (f*x.q))) 0 r_1.rea) in
      arr.(i) <- arr.(i) + f * cprod - c;
      v1
      )
    )
  in nest pat1 k
;;

let search_max_prod_bin tbl pat0 pat1 vmax =

  let rec nest imin imax =
  if (imin == imax) || (imin +1 == imax)
  then imin
  else (
    let r = search_k tbl pat0 pat1 ((imin + imax)/2) in
    Printf.printf "(%d - %d): %d \t " imin imax r;
    print_newline ();

    if r > vmax
    then nest imin ((imin + imax)/2)
    else nest ((imin + imax)/2) imax
    )
  in nest 1 20000000
;;


search_max_prod_bin tbl "ORE" "FUEL" 1000000000000;;

(**)
