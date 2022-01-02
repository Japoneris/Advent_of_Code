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

let lines = read_file "AOC_2020/input_04";;


let convert_to_list lst =
  let rec nest mem = function
  | [] -> mem
  | x::li -> nest ((String.split_on_char ' ' x)@mem) li
  in nest [] lst
;;

(*Put together items*)
let assemble lst =
  let rec nest mem tmp = function
  | [] -> tmp::mem
  | x::li ->
  if String.length x == 0
  then nest (tmp::mem) [] li
  else nest mem (x::tmp) li
  in nest [] [] lst
;;


let good_lines = convert_to_list lines;;
let good_elem = assemble good_lines;;

(*

    byr (Birth Year)
    iyr (Issue Year)
    eyr (Expiration Year)
    hgt (Height)
    hcl (Hair Color)
    ecl (Eye Color)
    pid (Passport ID)
    cid (Country ID)
*)
let check_valid lst =
  let arr = Array.make 8 0 in

  let item_lst = [
    ("byr", 0) ;
    ("iyr", 1);
    ("eyr", 2);
    ("hgt", 3);
    ("hcl", 4);
    ("ecl", 5);
    ("pid", 6);
    ("cid", 7)
  ] in

  let rec nest a = function
  | [] -> ()
  | (vi, i)::li ->
  if String.equal vi a
  then arr.(i) <- 1
  else nest a li
  in

  List.iter (fun x ->
    let (a:: _) = String.split_on_char  ':' x in
    nest a item_lst
    ) lst;

  let s = Array.fold_left (+) 0 arr in
  if s == 8 then true
  else if (s == 7) && arr.(7) == 0
  then true
  else false
;;

List.fold_left (fun mem x ->
  if check_valid x
  then mem + 1
  else mem
  ) 0 good_elem;;


let valid_element_1 = List.fold_left (fun mem x ->
  if check_valid x
  then x::mem
  else mem
  ) [] good_elem;;

(*

PART 2

    byr (Birth Year) - four digits; at least 1920 and at most 2002.
    iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    hgt (Height) - a number followed by either cm or in:
        If cm, the number must be at least 150 and at most 193.
        If in, the number must be at least 59 and at most 76.
    hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    pid (Passport ID) - a nine-digit number, including leading zeroes.
    cid (Country ID) - ignored, missing or not.
*)

let check_byr x =
  let y = Scanf.sscanf x "byr:%d" (fun x -> x) in
  (y >= 1920) && (y <= 2002)
;;

let check_iyr x =
  let y = Scanf.sscanf x "iyr:%d" (fun x -> x) in
  (y >= 2010) && (y <= 2020)
;;

let check_eyr x =
  let y = Scanf.sscanf x "eyr:%d" (fun x -> x) in
  (y >= 2020) && (y <= 2030)
;;

let check_hgt x =
  let (v, u) = Scanf.sscanf x "hgt:%d%s" (fun x y -> (x, y)) in
  if String.equal u "cm"
  then (v >= 150) && (v <= 193)
  else if String.equal u "in"
  then (v >= 59) && (v <= 76)
  else false
;;

let check_hexa stri =
  let l = String.length stri in
  let rec nest c = function
  | [] -> false
  | ii::li -> if c == ii then true
  else nest c li
  in

  let rec nest2 i =
  if i == l then true
  else if nest stri.[i] ['a'; 'b'; 'c'; 'd'; 'e'; 'f';
    '0'; '1'; '2'; '3'; '4'; '5'; '6';'7'; '8';'9']
  then nest2 (i+1)
  else false
  in nest2 0
  ;;

let check_numeral stri =
  let l = String.length stri in
  let rec nest c = function
  | [] -> false
  | ii::li -> if c == ii then true
  else nest c li
  in

  let rec nest2 i =
  if i == l then true
  else if nest stri.[i] ['0'; '1'; '2'; '3'; '4'; '5'; '6';'7'; '8';'9']
  then nest2 (i+1)
  else false
  in nest2 0
  ;;


let check_hcl x =
  let y = Scanf.sscanf x "hcl:#%s" (fun x -> x) in
  if String.length y == 6
  then check_hexa y
  else false
;;

let check_ecl x =
  let y = Scanf.sscanf x "ecl:%s" (fun x -> x) in
  let rec nest = function
  | [] -> false
  | x::li -> if String.equal x y then true
  else nest li
  in nest ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
;;

let check_pid x =
  let y = Scanf.sscanf x "pid:%s" (fun x -> x) in
  if String.length y == 9
  then check_numeral y
  else false
;;

let check_cid x = true;;

let check_fields lst =
  let item_lst = [
    ("byr", check_byr);
    ("eyr", check_eyr);
    ("iyr", check_iyr);
    ("hgt", check_hgt);
    ("hcl", check_hcl);
    ("ecl", check_ecl);
    ("pid", check_pid);
    ("cid", check_cid);
    ] in

  let rec nest a x = function
  | [] -> false
  | (vi, fx)::li ->
  if String.equal vi a
  then fx x
  else nest a x li
  in

  List.map (fun x ->
    try
      let (a:: _) = String.split_on_char  ':' x in
      nest a x item_lst
    with e ->
      false
    ) lst
;;

let is_okay pass =
  let ele = check_fields pass in
  List.fold_left (fun mem x -> mem && x) true ele
;;

List.fold_left (fun mem x ->
  if is_okay x
  then mem + 1
  else mem
  ) 0 valid_element_1;;




let test_good = [
"pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980";
"hcl:#623a2f";
"";
"eyr:2029 ecl:blu cid:129 byr:1989";
"iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm";
"";
"hcl:#888785";
"hgt:164cm byr:2001 iyr:2015 cid:88";
"pid:545766238 ecl:hzl";
"eyr:2022";
"iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"];;

let test_invalid = [
"eyr:1972 cid:100";
"hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926";
"";
"iyr:2019";
"hcl:#602927 eyr:1967 hgt:170cm";
"ecl:grn pid:012533040 byr:1946";
"";
"hcl:dab227 iyr:2012";
"ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277";
"";
"hgt:59cm ecl:zzz";
"eyr:2038 hcl:74454a iyr:2023";
"pid:3556412378 byr:2007";
];;

let test_good1 = convert_to_list test_good;;
let test_good1 = convert_to_list test_invalid;;
let test_good2 = assemble test_good1;;


let extract_field lst  stx =
  let rec nest = function
  | [] -> ""
  | x::lx ->
    let (a:: _) = String.split_on_char  ':' x in
    if String.equal a stx
    then x else nest lx
  in nest lst
  ;;
