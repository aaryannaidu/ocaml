#use "main.ml";;
open BigNum;;

(* ---- infrastructure ---- *)
let pass = ref 0
let fail = ref 0

let check label expected got =
  if expected = got then begin
    Printf.printf "[PASS] %s\n" label;
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

(* verify: quotient * divisor + remainder = dividend *)
let check_div_inv label dividend divisor =
  if divisor = bi 0 then ()
  else begin
    let q  = qt_big  dividend divisor in
    let r  = rem_big dividend divisor in
    (* q * divisor + r should equal dividend *)
    let reconstructed = add_big (mul_big q divisor) r in
    if reconstructed = dividend then
      (Printf.printf "[PASS] div-inv %s\n" label; incr pass)
    else
      (Printf.printf "[FAIL] div-inv %s\n       %s / %s  q=%s r=%s  recon=%s\n"
        label (show dividend) (show divisor) (show q) (show r) (show reconstructed);
       incr fail)
  end

(* ================================================================ *)
(*  int_to_bigint / bigint_to_string                                *)
(* ================================================================ *)
let () = print_endline "\n=== int_to_bigint / bigint_to_string ===";;
let _ = check "0"      "0"      (show (int_to_bigint 0));;
let _ = check "1"      "1"      (show (int_to_bigint 1));;
let _ = check "9"      "9"      (show (int_to_bigint 9));;
let _ = check "10"     "10"     (show (int_to_bigint 10));;
let _ = check "123"    "123"    (show (int_to_bigint 123));;
let _ = check "-1"     "-1"     (show (int_to_bigint (-1)));;
let _ = check "-456"   "-456"   (show (int_to_bigint (-456)));;
let _ = check "1000"   "1000"   (show (int_to_bigint 1000));;
let _ = check "99999"  "99999"  (show (int_to_bigint 99999));;

(* ================================================================ *)
(*  Addition                                                        *)
(* ================================================================ *)
let () = print_endline "\n=== Addition ===";;
let _ = check "0+0"           "0"      (show (add_big (bi 0)     (bi 0)));;
let _ = check "1+0"           "1"      (show (add_big (bi 1)     (bi 0)));;
let _ = check "0+1"           "1"      (show (add_big (bi 0)     (bi 1)));;
let _ = check "123+456"       "579"    (show (add_big (bi 123)   (bi 456)));;
let _ = check "999+1"         "1000"   (show (add_big (bi 999)   (bi 1)));;
let _ = check "9999+1"        "10000"  (show (add_big (bi 9999)  (bi 1)));;
let _ = check "-3+(-5)"       "-8"     (show (add_big (bi (-3))  (bi (-5))));;
let _ = check "10+(-3)"       "7"      (show (add_big (bi 10)    (bi (-3))));;
let _ = check "-10+3"         "-7"     (show (add_big (bi (-10)) (bi 3)));;
let _ = check "5+(-5)"        "0"      (show (add_big (bi 5)     (bi (-5))));;
let _ = check "(-100)+99"     "-1"     (show (add_big (bi (-100))(bi 99)));;
let _ = check "(-99)+100"     "1"      (show (add_big (bi (-99)) (bi 100)));;

(* ================================================================ *)
(*  Subtraction                                                     *)
(* ================================================================ *)
let () = print_endline "\n=== Subtraction ===";;
let _ = check "0-0"           "0"      (show (sub_big (bi 0)    (bi 0)));;
let _ = check "5-0"           "5"      (show (sub_big (bi 5)    (bi 0)));;
let _ = check "456-123"       "333"    (show (sub_big (bi 456)  (bi 123)));;
let _ = check "123-456"       "-333"   (show (sub_big (bi 123)  (bi 456)));;
let _ = check "100-100"       "0"      (show (sub_big (bi 100)  (bi 100)));;
let _ = check "1000-1"        "999"    (show (sub_big (bi 1000) (bi 1)));;
let _ = check "-5-(-3)"       "-2"     (show (sub_big (bi (-5)) (bi (-3))));;
let _ = check "-3-(-5)"       "2"      (show (sub_big (bi (-3)) (bi (-5))));;
let _ = check "5-(-3)"        "8"      (show (sub_big (bi 5)    (bi (-3))));;
let _ = check "-5-3"          "-8"     (show (sub_big (bi (-5)) (bi 3)));;

(* ================================================================ *)
(*  Multiplication — basic                                          *)
(* ================================================================ *)
let () = print_endline "\n=== Multiplication (basic) ===";;
let _ = check "0*0"           "0"       (show (mul_big (bi 0)    (bi 0)));;
let _ = check "1*1"           "1"       (show (mul_big (bi 1)    (bi 1)));;
let _ = check "0*999"         "0"       (show (mul_big (bi 0)    (bi 999)));;
let _ = check "999*0"         "0"       (show (mul_big (bi 999)  (bi 0)));;
let _ = check "1*999"         "999"     (show (mul_big (bi 1)    (bi 999)));;
let _ = check "999*1"         "999"     (show (mul_big (bi 999)  (bi 1)));;
let _ = check "12*13"         "156"     (show (mul_big (bi 12)   (bi 13)));;
let _ = check "99*99"         "9801"    (show (mul_big (bi 99)   (bi 99)));;
let _ = check "123*456"       "56088"   (show (mul_big (bi 123)  (bi 456)));;
let _ = check "1000*1000"     "1000000" (show (mul_big (bi 1000) (bi 1000)));;

(* ================================================================ *)
(*  Multiplication — sign corner cases                              *)
(* ================================================================ *)
let () = print_endline "\n=== Multiplication (signs) ===";;
let _ = check "(-7)*8"        "-56"     (show (mul_big (bi (-7))  (bi 8)));;
let _ = check "7*(-8)"        "-56"     (show (mul_big (bi 7)     (bi (-8))));;
let _ = check "(-7)*(-8)"     "56"      (show (mul_big (bi (-7))  (bi (-8))));;
let _ = check "(-1)*(-1)"     "1"       (show (mul_big (bi (-1))  (bi (-1))));;
let _ = check "123*(-789)"    "-97047"  (show (mul_big (bi 123)   (bi (-789))));;
(* commutativity *)
let _ = check "mul commut 1"  (show (mul_big (bi 37) (bi 49))) (show (mul_big (bi 49) (bi 37)));;
let _ = check "mul commut 2"  (show (mul_big (bi 123) (bi 456))) (show (mul_big (bi 456) (bi 123)));;

(* ================================================================ *)
(*  Multiplication — large numbers                                  *)
(* ================================================================ *)
let () = print_endline "\n=== Multiplication (large) ===";;
(* 999 * 999 = 998001 *)
let _ = check "999*999"       "998001"  (show (mul_big (bi 999)   (bi 999)));;
(* 9999 * 9999 = 99980001 *)
let _ = check "9999*9999"     "99980001" (show (mul_big (bi 9999)  (bi 9999)));;
(* cross-check: 111 * 111 = 12321 *)
let _ = check "111*111"       "12321"   (show (mul_big (bi 111)   (bi 111)));;
(* 2^10 = 1024 *)
let two = bi 2;;
let pow2_10 = List.fold_left mul_big (bi 1) (List.init 10 (fun _ -> two));;
let _ = check "2^10" "1024" (show pow2_10);;

(* ================================================================ *)
(*  Division / Remainder — basic                                    *)
(* ================================================================ *)
let () = print_endline "\n=== Division / Remainder (basic) ===";;
let _ = check "0/1   q"  "0"   (show (qt_big  (bi 0)    (bi 1)));;
let _ = check "0/1   r"  "0"   (show (rem_big (bi 0)    (bi 1)));;
let _ = check "1/1   q"  "1"   (show (qt_big  (bi 1)    (bi 1)));;
let _ = check "1/1   r"  "0"   (show (rem_big (bi 1)    (bi 1)));;
let _ = check "7/1   q"  "7"   (show (qt_big  (bi 7)    (bi 1)));;
let _ = check "7/7   q"  "1"   (show (qt_big  (bi 7)    (bi 7)));;
let _ = check "7/7   r"  "0"   (show (rem_big (bi 7)    (bi 7)));;
let _ = check "100/7 q"  "14"  (show (qt_big  (bi 100)  (bi 7)));;
let _ = check "100/7 r"  "2"   (show (rem_big (bi 100)  (bi 7)));;
let _ = check "7/100 q"  "0"   (show (qt_big  (bi 7)    (bi 100)));;
let _ = check "7/100 r"  "7"   (show (rem_big (bi 7)    (bi 100)));;
let _ = check "999/1 q"  "999" (show (qt_big  (bi 999)  (bi 1)));;
let _ = check "999/1 r"  "0"   (show (rem_big (bi 999)  (bi 1)));;
let _ = check "56088/456 q" "123" (show (qt_big  (bi 56088) (bi 456)));;
let _ = check "56088/456 r" "0"   (show (rem_big (bi 56088) (bi 456)));;
let _ = check "10000/3 q" "3333" (show (qt_big (bi 10000) (bi 3)));;
let _ = check "10000/3 r" "1"    (show (rem_big (bi 10000) (bi 3)));;

(* ================================================================ *)
(*  Division — invariant: q * d + r = n for many cases             *)
(* ================================================================ *)
let () = print_endline "\n=== Division invariant (q*d+r = n) ===";;
let _ = check_div_inv "100/7"     (bi 100)   (bi 7);;
let _ = check_div_inv "97047/789" (bi 97047) (bi 789);;
let _ = check_div_inv "9999/99"   (bi 9999)  (bi 99);;
let _ = check_div_inv "10000/3"   (bi 10000) (bi 3);;
let _ = check_div_inv "1/9"       (bi 1)     (bi 9);;
let _ = check_div_inv "99999/7"   (bi 99999) (bi 7);;
let _ = check_div_inv "100000/11" (bi 100000)(bi 11);;
let _ = check_div_inv "1/1"       (bi 1)     (bi 1);;
let _ = check_div_inv "0/5"       (bi 0)     (bi 5);;
let _ = check_div_inv "5/5"       (bi 5)     (bi 5);;

(* ================================================================ *)
(*  Division — sign cases                                           *)
(* ================================================================ *)
let () = print_endline "\n=== Division (signs) ===";;
let _ = check "(-17)/5  q"  "-3"  (show (qt_big  (bi (-17)) (bi 5)));;
let _ = check "(-17)/5  r"  "-2"  (show (rem_big (bi (-17)) (bi 5)));;
let _ = check "17/(-5)  q"  "-3"  (show (qt_big  (bi 17)    (bi (-5))));;
let _ = check "17/(-5)  r"  "2"   (show (rem_big (bi 17)    (bi (-5))));;
let _ = check "(-17)/(-5) q" "3"  (show (qt_big  (bi (-17)) (bi (-5))));;
let _ = check "(-17)/(-5) r" "-2" (show (rem_big (bi (-17)) (bi (-5))));;

(* ================================================================ *)
(*  Division — by zero                                              *)
(* ================================================================ *)
let () = print_endline "\n=== Division by zero ===";;
let _ =
  (try let _ = qt_big (bi 42) (bi 0) in check "qt div-by-zero" "exception" "no exception"
   with Division_by_zero -> check "qt div-by-zero" "exception" "exception");;
let _ =
  (try let _ = rem_big (bi 42) (bi 0) in check "rem div-by-zero" "exception" "no exception"
   with Division_by_zero -> check "rem div-by-zero" "exception" "exception");;

(* ================================================================ *)
(*  Negation & Absolute value                                       *)
(* ================================================================ *)
let () = print_endline "\n=== Negation / Abs ===";;
let _ = check "neg 0"   "0"   (show (neg_big (bi 0)));;
let _ = check "neg 5"   "-5"  (show (neg_big (bi 5)));;
let _ = check "neg -5"  "5"   (show (neg_big (bi (-5))));;
let _ = check "abs 0"   "0"   (show (abs_big (bi 0)));;
let _ = check "abs 42"  "42"  (show (abs_big (bi 42)));;
let _ = check "abs -42" "42"  (show (abs_big (bi (-42))));;
(* double-neg = identity *)
let _ = check "neg(neg 7)" "7" (show (neg_big (neg_big (bi 7))));;

(* ================================================================ *)
(*  Comparisons                                                     *)
(* ================================================================ *)
let () = print_endline "\n=== Equality ===";;
let _ = checkb "eq 0 0"   T (eq_big (bi 0)    (bi 0));;
let _ = checkb "eq 5 5"   T (eq_big (bi 5)    (bi 5));;
let _ = checkb "eq 5 6"   F (eq_big (bi 5)    (bi 6));;
let _ = checkb "eq -5 -5" T (eq_big (bi (-5)) (bi (-5)));;
let _ = checkb "eq -5 5"  F (eq_big (bi (-5)) (bi 5));;

let () = print_endline "\n=== gt_big (strict >) ===";;
let _ = checkb "gt 6 5"   T (gt_big (bi 6)    (bi 5));;
let _ = checkb "gt 5 6"   F (gt_big (bi 5)    (bi 6));;
let _ = checkb "gt 5 5"   F (gt_big (bi 5)    (bi 5));;   (* equal → F *)
let _ = checkb "gt 1 -1"  T (gt_big (bi 1)    (bi (-1)));;
let _ = checkb "gt -1 1"  F (gt_big (bi (-1)) (bi 1));;
let _ = checkb "gt -3 -5" T (gt_big (bi (-3)) (bi (-5)));;  (* -3 > -5 *)
let _ = checkb "gt -5 -3" F (gt_big (bi (-5)) (bi (-3)));;
let _ = checkb "gt -5 -5" F (gt_big (bi (-5)) (bi (-5)));; (* equal → F *)

let () = print_endline "\n=== lt_big (strict <) ===";;
let _ = checkb "lt 5 6"   T (lt_big (bi 5)    (bi 6));;
let _ = checkb "lt 6 5"   F (lt_big (bi 6)    (bi 5));;
let _ = checkb "lt 5 5"   F (lt_big (bi 5)    (bi 5));;   (* equal → F *)
let _ = checkb "lt -5 -3" T (lt_big (bi (-5)) (bi (-3)));; (* -5 < -3 *)
let _ = checkb "lt -3 -5" F (lt_big (bi (-3)) (bi (-5)));;
let _ = checkb "lt -3 -3" F (lt_big (bi (-3)) (bi (-3)));;
let _ = checkb "lt -1 0"  T (lt_big (bi (-1)) (bi 0));;
let _ = checkb "lt 0 1"   T (lt_big (bi 0)    (bi 1));;

let () = print_endline "\n=== ge_big (>=) ===";;
let _ = checkb "ge 5 5"   T (ge_big (bi 5)    (bi 5));;   (* equal → T *)
let _ = checkb "ge 6 5"   T (ge_big (bi 6)    (bi 5));;
let _ = checkb "ge 4 5"   F (ge_big (bi 4)    (bi 5));;
let _ = checkb "ge -3 -5" T (ge_big (bi (-3)) (bi (-5)));;
let _ = checkb "ge -5 -3" F (ge_big (bi (-5)) (bi (-3)));;
let _ = checkb "ge -5 -5" T (ge_big (bi (-5)) (bi (-5)));;

let () = print_endline "\n=== le_big (<=) ===";;
let _ = checkb "le 5 5"   T (le_big (bi 5)    (bi 5));;   (* equal → T *)
let _ = checkb "le 4 5"   T (le_big (bi 4)    (bi 5));;
let _ = checkb "le 6 5"   F (le_big (bi 6)    (bi 5));;
let _ = checkb "le -5 -3" T (le_big (bi (-5)) (bi (-3)));;
let _ = checkb "le -3 -5" F (le_big (bi (-3)) (bi (-5)));;
let _ = checkb "le -5 -5" T (le_big (bi (-5)) (bi (-5)));;

(* ================================================================ *)
(*  Factorial (stress test)                                          *)
(* ================================================================ *)
let () = print_endline "\n=== Factorial (Stress Test) ===";;
let rec factorial_big n = 
  if n <= 1 then bi 1
  else mul_big (bi n) (factorial_big (n - 1))

(* 10! = 3628800 *)
let _ = check "10!" "3628800" (show (factorial_big 10));;

(* 20! = 2432902008176640000 *)
let _ = check "20!" "2432902008176640000" (show (factorial_big 20));;

(* 50! = 30414093201713378043612608166064768844377641568960512000000000000 *)
let _ = check "50!" "30414093201713378043612608166064768844377641568960512000000000000" (show (factorial_big 50));;

(* 100-digit number test *)
let rec make_huge_num n d = 
  if n = 0 then [] else d :: make_huge_num (n-1) d

let huge1 = (NonNeg, make_huge_num 50 9)
let huge2 = (NonNeg, [1])

let _ = check "999...9 (50) + 1" ("1" ^ String.make 50 '0') (show (add_big huge1 huge2));;

let huge_sub = sub_big huge1 huge2
let _ = check "999...9 (50) - 1" ((String.make 49 '9') ^ "8") (show huge_sub);;

(* ================================================================ *)
(*  Summary                                                         *)
(* ================================================================ *)
let () =
  Printf.printf "\n============================\n";
  Printf.printf " Results: %d passed, %d failed\n" !pass !fail;
  Printf.printf "============================\n";;
