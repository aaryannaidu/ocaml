#use "main.ml";;
open BigNum;;

(* ---- infrastructure ---- *)
let pass = ref 0
let fail = ref 0

let check label expected got =
  if expected = got then begin
    Printf.printf "[PASS] %s\n       expected : %s\n       got      : %s\n" label expected got;
    incr pass
  end else begin
    Printf.printf "[FAIL] %s\n       expected : %s\n       got      : %s\n" label expected got;
    incr fail
  end

let checkb label expected got =
  let s = function T -> "T" | F -> "F" in
  check label (s expected) (s got)

(* build bigint from OCaml int *)
let bi n =
  let digits m =
    if m = 0 then [0]
    else
      let rec aux x acc = if x = 0 then acc else aux (x / 10) ((x mod 10) :: acc)
      in aux m []
  in
  if n >= 0 then (NonNeg, digits n) else (Neg, digits (-n))

let show = bigint_to_string



(* ================================================================ *)
(*  A2 DEMO TESTCASES: SECTION 2.1 BIGINT                           *)
(* ================================================================ *)

let () = print_endline "\n=== 2.1.1 Conversion, pretty-print, unary negation, absolute value ===";;
let _ = check "conv 0" "0" (show (int_to_bigint 0));;
let _ = check "conv 7" "7" (show (int_to_bigint 7));;
let _ = check "conv -42" "-42" (show (int_to_bigint (-42)));;
let _ = check "conv 120030" "120030" (show (int_to_bigint 120030));;

let _ = check "neg 0" "0" (show (neg_big (int_to_bigint 0)));;
let _ = check "neg 42" "-42" (show (neg_big (int_to_bigint 42)));;
let _ = check "abs -98765" "98765" (show (abs_big (int_to_bigint (-98765))));;

let () = print_endline "\n=== 2.1.2 Addition and subtraction, including carries and signed cancellation ===";;
let a_digits = List.init 20 (fun _ -> 9)
let a = (NonNeg, a_digits)
let b = int_to_bigint 1
let c = int_to_bigint 5000
let d = int_to_bigint (-5000)
let e = int_to_bigint 12345
let f = int_to_bigint 54321

let _ = check "A + B" ("1" ^ String.make 20 '0') (show (add_big a b));;
let _ = check "A - A" "0" (show (sub_big a a));;
let _ = check "C + D" "0" (show (add_big c d));;
let _ = check "E - F" "-41976" (show (sub_big e f));;
let _ = check "F - E" "41976" (show (sub_big f e));;

let () = print_endline "\n=== 2.1.3 Multiplication, including sign and carry propagation ===";;
let _ = check "12345 * (-6789)" "-83810205" (show (mul_big (int_to_bigint 12345) (int_to_bigint (-6789))));;
let _ = check "99999 * 99999" "9999800001" (show (mul_big (int_to_bigint 99999) (int_to_bigint 99999)));;
let _ = check "0 * 987654321" "0" (show (mul_big (int_to_bigint 0) (int_to_bigint 987654321)));;
let _ = check "(-12) * (-13)" "156" (show (mul_big (int_to_bigint (-12)) (int_to_bigint (-13))));;

let () = print_endline "\n=== 2.1.4 Quotient and remainder for positive inputs ===";;
let _ = check "quotient(123456789, 12345)" "10000" (show (qt_big (int_to_bigint 123456789) (int_to_bigint 12345)));;
let _ = check "remainder(123456789, 12345)" "6789" (show (rem_big (int_to_bigint 123456789) (int_to_bigint 12345)));;
let _ = check "quotient(9876543210, 97)" "101820033" (show (qt_big (int_to_bigint 9876543210) (int_to_bigint 97)));;
let _ = check "remainder(9876543210, 97)" "9" (show (rem_big (int_to_bigint 9876543210) (int_to_bigint 97)));;

let () = print_endline "\n=== 2.1.5 Comparison operations ===";;
let do_comps label x y =
  let bx = int_to_bigint x in
  let by = int_to_bigint y in
  let p expected_op name fn =
    let ex = expected_op x y in
    checkb (label ^ " " ^ name) (if ex then T else F) (fn bx by)
  in
  p (=) "=" eq_big;
  p (>) ">" gt_big;
  p (<) "<" lt_big;
  p (>=) ">=" ge_big;
  p (<=) "<=" le_big

let _ = do_comps "12345 and 12345" 12345 12345;;
let _ = do_comps "-100 and 0" (-100) 0;;
let _ = do_comps "-5 and -10" (-5) (-10);;
let _ = do_comps "99999 and 100000" 99999 100000;;

let () = print_endline "\n=== 2.1.6 Undefined arithmetic operation ===";;
let _ =
  try let _ = qt_big (int_to_bigint 12345) (int_to_bigint 0) in check "quotient(12345, 0)" "exception" "no exception"
  with Division_by_zero -> check "quotient(12345, 0)" "exception" "exception"
let _ =
  try let _ = rem_big (int_to_bigint 12345) (int_to_bigint 0) in check "remainder(12345, 0)" "exception" "no exception"
  with Division_by_zero -> check "remainder(12345, 0)" "exception" "exception"

(* ================================================================ *)
(*  Summary                                                         *)
(* ================================================================ *)
let () =
  Printf.printf "\n============================\n";
  Printf.printf " Results: %d passed, %d failed\n" !pass !fail;
  Printf.printf "============================\n";;
