
let init = [6;13;1;15;2;0]

let solve_1 lst rmax =
  let a1 = Array.make 10000000 0 in

  List.iteri (fun i x -> a1.(x) <- i+1) lst;
  (*
  r: turn number
  s: what to call next
  *)
  let rec nest r s =
    if r == rmax
    then s
    else (
      let v = a1.(s) in
      a1.(s) <- r;
      Printf.printf "Round %d: \t%d => %d" r s v;
      print_newline ();
        if v == 0
      then nest (r+1) 0
      else nest (r+1) (r-v)
      )
  in nest (1 + List.length lst) 0
;;

solve_1 [6;13;1;15;2;0] 2020;;

(*Same, but do not speak often*)
let solve_2 lst rmax =
  let a1 = Array.make 100000000 0 in

  List.iteri (fun i x -> a1.(x) <- i+1) lst;
  (*
  r: turn number
  s: what to call next
  *)
  let rec nest r s =
    if r == rmax
    then s
    else (
      let v = a1.(s) in
      a1.(s) <- r;
      if r mod 1000 == 0
      then (

      Printf.printf "Round %d: \t%d => %d" r s v;
      print_newline ();
      );
        if v == 0
      then nest (r+1) 0
      else nest (r+1) (r-v)
      )
  in nest (1 + List.length lst) 0
;;
