let scanint x = Scanf.sscanf x "%d " (fun y -> y);;

let read_input_data () =
  let ic = open_in "input_02" in

  let stri  = String.split_on_char ',' (input_line ic) in
  close_in_noerr ic;
  Array.of_list (List.map (scanint) stri)
  ;;


let solve data =
  let rec nest i =
    let vi = data.(i) in
    if vi == 1
    then (
      let a = data.(data.(i+1))
      and b = data.(data.(i+2)) in
      data.(data.(i+3)) <- a + b;
      nest (i+4)
      )
    else if vi == 99
    then data
    else if vi == 2
    then (
      let a = data.(data.(i+1))
      and b = data.(data.(i+2)) in
      data.(data.(i+3)) <- a * b;
      nest (i+4)
      )
    else (
      Printf.printf "Error: %d\n" vi;
      data
      )
    in nest 0
;;




let data = read_input_data ();;
data.(1) <- 12;;
data.(2) <- 2;;
let result = solve data;;


let part_2 v =
  let rec nest noun verb =
    if noun == 100
    then nest 0 (verb+1)
    else if verb == 100
    then (noun, verb)
    else (

      let data = read_input_data () in
      data.(1) <- noun;
      data.(2) <- verb;
      let res = solve data in
      if res.(0) == v
      then (noun, verb)
      else (
        nest (noun+1) verb
        )
    ) in nest 0 0;;

let (a, b) = part_2  19690720;;
Printf.printf "Day 2 part 2: %d\n" (a * 100 + b)
