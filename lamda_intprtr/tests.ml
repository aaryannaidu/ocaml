open Ast

(* ── Church encodings ─────────────────────────────────────────────────────── *)

let c_true  = Lam ("t", Lam ("f", Var "t"))
let c_false = Lam ("t", Lam ("f", Var "f"))
let c_not   = Lam ("b", App (App (Var "b", c_false), c_true))

let c_pair = Lam ("x", Lam ("y", Lam ("z", App (App (Var "z", Var "x"), Var "y"))))
let c_fst  = Lam ("p", App (Var "p", c_true))
let c_snd  = Lam ("p", App (Var "p", c_false))

let c_0 = Lam ("f", Lam ("x", Var "x"))
let c_1 = Lam ("f", Lam ("x", App (Var "f", Var "x")))
let c_2 = Lam ("f", Lam ("x", App (Var "f", App (Var "f", Var "x"))))
let c_3 = Lam ("f", Lam ("x", App (Var "f", App (Var "f", App (Var "f", Var "x")))))
let c_add = Lam ("m", Lam ("n", Lam ("f", Lam ("x",
    App (App (Var "m", Var "f"), App (App (Var "n", Var "f"), Var "x"))))))

(* ── Test cases ───────────────────────────────────────────────────────────── *)

(* 1. Identity: (\x.x) 42 → 42 *)
let test_id = App (Lam ("x", Var "x"), Int 42)

(* 2. K combinator: (\x.\y.x) 1 2 → 1 *)
let test_k = App (App (Lam ("x", Lam ("y", Var "x")), Int 1), Int 2)

(* 3. Church NOT: (not true) → false = \t.\f.f *)
let test_not_true = App (c_not, c_true)

(* 4. Church Pair: fst (pair 100 200) → 100 *)
let test_pair_fst = App (c_fst, App (App (c_pair, Int 100), Int 200))

(* 5. Church Pair: snd (pair 100 200) → 200 *)
let test_pair_snd = App (c_snd, App (App (c_pair, Int 100), Int 200))

(* 6. Church Numerals: (1 + 2) applied as succ-from-0 → 3
   c_add c_1 c_2 applied to (\x. x+1) starting at 0 gives 3 *)
let test_church_add =
  App (App (App (App (c_add, c_1), c_2), Lam ("x", Add (Var "x", Int 1))), Int 0)

(* 7. Church Zero check: c_0 applied to (\x. x+1) from 0 → 0 (applied 0 times) *)
let test_church_zero =
  App (App (c_0, Lam ("x", Add (Var "x", Int 1))), Int 0)

(* 8. Mixed — Church numeral driving a primitive:
   c_3 applied to (\x. x*2) starting at 1 → 1*2*2*2 = 8 *)
let test_mixed_church_int =
  App (App (c_3, Lam ("x", Mul (Var "x", Int 2))), Int 1)

(* 9. Nested primitives: (2*5) + (10-2) → 18 *)
let test_arithmetic = Add (Mul (Int 2, Int 5), Sub (Int 10, Int 2))

(* 10. Nested control flow:
   if (true && (3 < 10)) then (if false then 0 else 5) * 4 else 0 → 20 *)
let test_nested_if =
  If (And (Bool true, Lt (Int 3, Int 10)),
      Mul (If (Bool false, Int 0, Int 5), Int 4),
      Int 0)

(* 11. Lazy evaluation — Krivine skips Omega, SECD loops:
   (\y.99) Ω — Krivine returns 99; SECD would loop forever *)
let omega     = App (Lam ("x", App (Var "x", Var "x")), Lam ("x", App (Var "x", Var "x")))
let test_lazy = App (Lam ("y", Int 99), omega)

(* ── Printer helpers ──────────────────────────────────────────────────────── *)

let secd_l_str = function
  | Secd.VTerm_l t          -> Ast.to_string t
  | Secd.VClos_l (x, _, _)  -> "<clos:\\" ^ x ^ ">"

let secd_f_str = function
  | Secd.VTerm_f t          -> Ast.to_string t
  | Secd.VClos_f (x, _, _)  -> "<clos:\\" ^ x ^ ">"

let check result expected =
  if result = expected then result ^ "  [PASS]"
  else result ^ "  [FAIL — got: " ^ result ^ "]"

(* kexp = expected string for both Krivine variants
   sexp = expected string for both SECD variants
   skip_secd = true for tests that would loop forever in eager evaluation *)
let run_test name term kexp sexp skip_secd =
  print_endline ("\n[ " ^ name ^ " ]");
  print_endline ("  Expected (Krivine) : " ^ kexp);
  if not skip_secd then print_endline ("  Expected (SECD)    : " ^ sexp)
  else print_endline "  Expected (SECD)    : [SKIPPED]";
  print_endline "  ------------------------------------------";

  print_string "  Krivine (list)     : ";
  (try print_endline (check (Ast.to_string (Krivine.eval_list term)) kexp)
   with e -> print_endline ("ERROR — " ^ Printexc.to_string e));

  print_string "  Krivine (func)     : ";
  (try print_endline (check (Ast.to_string (Krivine.eval_func term)) kexp)
   with e -> print_endline ("ERROR — " ^ Printexc.to_string e));

  if skip_secd then (
    print_endline "  SECD    (list)     : [SKIPPED — would loop forever in eager eval]";
    print_endline "  SECD    (func)     : [SKIPPED — would loop forever in eager eval]"
  ) else (
    print_string "  SECD    (list)     : ";
    (try print_endline (check (secd_l_str (Secd.run_list term)) sexp)
     with e -> print_endline ("ERROR — " ^ Printexc.to_string e));

    print_string "  SECD    (func)     : ";
    (try print_endline (check (secd_f_str (Secd.run_func term)) sexp)
     with e -> print_endline ("ERROR — " ^ Printexc.to_string e))
  )

(* ── Run all tests ────────────────────────────────────────────────────────── *)

let run_all () =
  print_endline "============================================";
  print_endline "       LAMBDA INTERPRETER TEST SUITE       ";
  print_endline "============================================";

  (* Basic lambda *)
  run_test "1. Identity: (\\x.x) 42"
    test_id "42" "42" false;

  run_test "2. K-combinator: (\\x.\\y.x) 1 2"
    test_k "1" "1" false;

  (* Church encodings *)
  run_test "3. Church NOT true  →  church_false"
    test_not_true "(\\t.(\\f.f))" "<clos:\\t>" false;

  run_test "4. Church Pair fst (pair 100 200)"
    test_pair_fst "100" "100" false;

  run_test "5. Church Pair snd (pair 100 200)"
    test_pair_snd "200" "200" false;

  run_test "6. Church Add: (1+2) as iterated succ from 0"
    test_church_add "3" "3" false;

  run_test "7. Church Zero: c_0 succ 0 = 0"
    test_church_zero "0" "0" false;

  (* Mixed church + primitives *)
  run_test "8. Mixed: Church_3 (*2) 1  →  8"
    test_mixed_church_int "8" "8" false;

  (* Pure primitives *)
  run_test "9. Arithmetic: (2*5) + (10-2)"
    test_arithmetic "18" "18" false;

  run_test "10. Nested If/And/Lt/Mul"
    test_nested_if "20" "20" false;

  (* CBN vs CBV *)
  run_test "11. Lazy: (\\y.99) Omega  [SECD skipped]"
    test_lazy "99" "" true;

  print_endline "\n============================================";
  print_endline "                   DONE                    ";
  print_endline "============================================"
