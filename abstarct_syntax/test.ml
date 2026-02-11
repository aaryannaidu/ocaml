(* Load the main file *)
#use "main.ml";;

open Signature;;
open Expression;;
open Substitution;;

(* ========================================
   TEST FRAMEWORK
   ======================================== *)

let test_count = ref 0;;
let passed_count = ref 0;;
let failed_count = ref 0;;

let assert_true name condition =
  test_count := !test_count + 1;
  if condition then begin
    passed_count := !passed_count + 1;
    Printf.printf "✓ PASS: %s\n" name
  end else begin
    failed_count := !failed_count + 1;
    Printf.printf "✗ FAIL: %s\n" name
  end;;

let assert_false name condition =
  assert_true name (not condition);;

let assert_equal name expected actual =
  test_count := !test_count + 1;
  if expected = actual then begin
    passed_count := !passed_count + 1;
    Printf.printf "✓ PASS: %s\n" name
  end else begin
    failed_count := !failed_count + 1;
    Printf.printf "✗ FAIL: %s (expected %d, got %d)\n" name expected actual
  end;;

let print_summary () =
  Printf.printf "\n========================================\n";
  Printf.printf "TEST SUMMARY\n";
  Printf.printf "========================================\n";
  Printf.printf "Total tests: %d\n" !test_count;
  Printf.printf "Passed: %d\n" !passed_count;
  Printf.printf "Failed: %d\n" !failed_count;
  if !failed_count = 0 then
    Printf.printf "✓ ALL TESTS PASSED!\n"
  else
    Printf.printf "✗ SOME TESTS FAILED\n";
  Printf.printf "========================================\n";;

(* ========================================
   TEST PART 1: SIGNATURES
   ======================================== *)

Printf.printf "=== TESTING SIGNATURES ===\n";;

(* Test 1: Valid signature *)
let sig1 = [("f", 2); ("g", 1); ("a", 0)];;
assert_true "Valid signature should pass" (check_sig sig1);;

(* Test 2: Duplicate symbols *)
let sig2 = [("f", 2); ("g", 1); ("f", 0)];;
assert_false "Duplicate symbols should fail" (check_sig sig2);;

(* Test 3: Negative arity *)
let sig3 = [("f", 2); ("g", -1); ("a", 0)];;
assert_false "Negative arity should fail" (check_sig sig3);;

(* Test 4: Empty signature *)
let sig4 = [];;
assert_true "Empty signature should pass" (check_sig sig4);;

(* Test 5: Single symbol *)
let sig5 = [("f", 3)];;
assert_true "Single symbol should pass" (check_sig sig5);;

Printf.printf "\n";;

(* ========================================
   TEST PART 2: EXPRESSIONS
   ======================================== *)

Printf.printf "=== TESTING EXPRESSIONS ===\n";;

(* Define a valid signature for testing *)
let test_sig = [("f", 2); ("g", 1); ("a", 0); ("h", 3)];;

(* Test expressions *)
let exp1 = V "x";;
assert_true "Variable should be well-formed" (wfexp test_sig exp1);;
assert_equal "Variable height should be 0" 0 (ht exp1);;
assert_equal "Variable size should be 1" 1 (size exp1);;
assert_true "Variable vars should contain x" (vars exp1 = ["x"]);;

let exp2 = Node (("a", 0), [||]);;
assert_true "Nullary symbol should be well-formed" (wfexp test_sig exp2);;
assert_equal "Nullary symbol height should be 0" 0 (ht exp2);;
assert_equal "Nullary symbol size should be 1" 1 (size exp2);;
assert_true "Nullary symbol should have no vars" (vars exp2 = []);;

let exp3 = Node (("g", 1), [| V "x" |]);;
assert_true "Unary function should be well-formed" (wfexp test_sig exp3);;
assert_equal "Unary function height should be 1" 1 (ht exp3);;
assert_equal "Unary function size should be 2" 2 (size exp3);;
assert_true "Unary function vars should contain x" (vars exp3 = ["x"]);;

let exp4 = Node (("f", 2), [| V "x"; V "y" |]);;
assert_true "Binary function should be well-formed" (wfexp test_sig exp4);;
assert_equal "Binary function height should be 1" 1 (ht exp4);;
assert_equal "Binary function size should be 3" 3 (size exp4);;
assert_true "Binary function vars should contain x and y" (vars exp4 = ["x"; "y"]);;

let exp5 = Node (("f", 2), [| Node (("g", 1), [| V "x" |]); Node (("a", 0), [||]) |]);;
assert_true "Nested expression should be well-formed" (wfexp test_sig exp5);;
assert_equal "Nested expression height should be 2" 2 (ht exp5);;
assert_equal "Nested expression size should be 4" 4 (size exp5);;
assert_true "Nested expression vars should contain x" (vars exp5 = ["x"]);;

let exp6 = Node (("h", 3), [| V "x"; V "y"; V "z" |]);;
assert_true "Ternary function should be well-formed" (wfexp test_sig exp6);;
assert_equal "Ternary function height should be 1" 1 (ht exp6);;
assert_equal "Ternary function size should be 4" 4 (size exp6);;
assert_true "Ternary function vars should contain x, y, z" (vars exp6 = ["x"; "y"; "z"]);;

let exp7 = Node (("f", 2), [| 
  Node (("f", 2), [| V "x"; V "y" |]); 
  Node (("g", 1), [| V "z" |]) 
|]);;
assert_true "Complex nested expression should be well-formed" (wfexp test_sig exp7);;
assert_equal "Complex nested expression height should be 2" 2 (ht exp7);;
assert_equal "Complex nested expression size should be 6" 6 (size exp7);;
assert_true "Complex nested expression vars should contain x, y, z" (vars exp7 = ["x"; "y"; "z"]);;

(* Test invalid expressions *)
let exp8 = Node (("f", 2), [| V "x" |]);;
assert_false "Wrong arity should fail" (wfexp test_sig exp8);;

let exp9 = Node (("k", 1), [| V "x" |]);;
assert_false "Unknown symbol should fail" (wfexp test_sig exp9);;

let exp10 = Node (("f", 2), [| V "x"; V "x" |]);;
assert_true "Duplicate variables should still be well-formed" (wfexp test_sig exp10);;
assert_true "Duplicate variables should return unique list" (vars exp10 = ["x"]);;

Printf.printf "\n";;

(* ========================================
   TEST PART 3: SUBSTITUTION
   ======================================== *)

Printf.printf "=== TESTING SUBSTITUTION ===\n";;

(* Helper function to compare expressions *)
let rec exp_equal e1 e2 =
  match (e1, e2) with
  | (V v1, V v2) -> v1 = v2
  | (Node ((n1, a1), args1), Node ((n2, a2), args2)) ->
      n1 = n2 && a1 = a2 && Array.length args1 = Array.length args2 &&
      Array.for_all (fun i -> exp_equal args1.(i) args2.(i)) 
        (Array.init (Array.length args1) (fun i -> i))
  | _ -> false;;

(* Test 1: Simple substitution *)
let exp_s1 = V "x";;
let s1 = [("x", Node (("a", 0), [||]))];;
let result1 = subst s1 exp_s1;;
let expected1 = Node (("a", 0), [||]);;
assert_true "Simple substitution x -> a()" (exp_equal result1 expected1);;

(* Test 2: Nested substitution *)
let exp_s2 = Node (("f", 2), [| V "x"; V "y" |]);;
let s2 = [("x", Node (("g", 1), [| V "z" |])); ("y", Node (("a", 0), [||]))];;
let result2 = subst s2 exp_s2;;
let expected2 = Node (("f", 2), [| Node (("g", 1), [| V "z" |]); Node (("a", 0), [||]) |]);;
assert_true "Nested substitution" (exp_equal result2 expected2);;

(* Test 3: Partial substitution *)
let exp_s3 = Node (("f", 2), [| V "x"; V "y" |]);;
let s3 = [("x", Node (("a", 0), [||]))];;
let result3 = subst s3 exp_s3;;
let expected3 = Node (("f", 2), [| Node (("a", 0), [||]); V "y" |]);;
assert_true "Partial substitution (only x)" (exp_equal result3 expected3);;

(* Test 4: No substitution *)
let exp_s4 = Node (("f", 2), [| V "x"; V "y" |]);;
let s4 = [];;
let result4 = subst s4 exp_s4;;
assert_true "Empty substitution should not change expression" (exp_equal result4 exp_s4);;

(* Test 5: Edit at position *)
let exp_edit = Node (("f", 2), [| Node (("g", 1), [| V "x" |]); Node (("a", 0), [||]) |]);;
let new_exp = Node (("a", 0), [||]);;
match edit [0; 0] new_exp exp_edit with
| Some result -> 
    let expected = Node (("f", 2), [| Node (("g", 1), [| Node (("a", 0), [||]) |]); Node (("a", 0), [||]) |]) in
    assert_true "Edit at position [0,0]" (exp_equal result expected)
| None -> assert_true "Edit at position [0,0] should succeed" false;;

match edit [0; 0; 0] new_exp exp_edit with
| Some _ -> assert_true "Edit at invalid position should fail" false
| None -> assert_true "Edit at invalid position [0,0,0] should return None" true;;

(* Test 6: In-place substitution *)
let exp_inplace = Node (("f", 2), [| V "x"; Node (("g", 1), [| V "y" |]) |]);;
let s_inplace = [("x", Node (("a", 0), [||])); ("y", V "z")];;
subst_inplace s_inplace exp_inplace;;
let expected_inplace = Node (("f", 2), [| Node (("a", 0), [||]); Node (("g", 1), [| V "z" |]) |]);;
assert_true "In-place substitution should mutate expression" (exp_equal exp_inplace expected_inplace);;

Printf.printf "\n";;

(* ========================================
   TEST PART 4: PREDICATES
   ======================================== *)

open Predicates;;

Printf.printf "=== TESTING PREDICATES ===\n";;

(* Define signatures *)
let exp_sig = [("f", 2); ("g", 1); ("a", 0); ("h", 3)];;
let pred_sig = [("P", 1); ("Q", 2); ("R", 0); ("S", 3)];;

(* Helper function to compare predicates *)
let rec pred_equal p1 p2 =
  match (p1, p2) with
  | (T, T) -> true
  | (F, F) -> true
  | (Pred ((n1, a1), args1), Pred ((n2, a2), args2)) ->
      n1 = n2 && a1 = a2 && Array.length args1 = Array.length args2 &&
      Array.for_all (fun i -> exp_equal args1.(i) args2.(i)) 
        (Array.init (Array.length args1) (fun i -> i))
  | (Not p1', Not p2') -> pred_equal p1' p2'
  | (And (p1a, p1b), And (p2a, p2b)) -> pred_equal p1a p2a && pred_equal p1b p2b
  | (Or (p1a, p1b), Or (p2a, p2b)) -> pred_equal p1a p2a && pred_equal p1b p2b
  | _ -> false;;

(* Test 1: Simple predicates well-formedness *)
assert_true "T should be well-formed" (wff pred_sig exp_sig T);;
assert_true "F should be well-formed" (wff pred_sig exp_sig F);;
assert_true "R (nullary) should be well-formed" (wff pred_sig exp_sig (Pred (("R", 0), [||])));;
assert_true "P(x) should be well-formed" (wff pred_sig exp_sig (Pred (("P", 1), [| V "x" |])));;

(* Test 2: Complex expressions in predicates *)
let p_complex = Pred (("P", 1), [| Node (("g", 1), [| V "x" |]) |]);;
assert_true "P(g(x)) should be well-formed" (wff pred_sig exp_sig p_complex);;

let p_complex2 = Pred (("Q", 2), [| V "x"; Node (("f", 2), [| V "y"; V "z" |]) |]);;
assert_true "Q(x, f(y, z)) should be well-formed" (wff pred_sig exp_sig p_complex2);;

(* Test 3: Compound predicates *)
let p_not = Not (Pred (("P", 1), [| V "x" |]));;
assert_true "¬P(x) should be well-formed" (wff pred_sig exp_sig p_not);;

let p_and = And (Pred (("P", 1), [| V "x" |]), Pred (("R", 0), [||]));;
assert_true "P(x) ∧ R should be well-formed" (wff pred_sig exp_sig p_and);;

let p_or = Or (Pred (("P", 1), [| V "x" |]), Not (Pred (("R", 0), [||])));;
assert_true "P(x) ∨ ¬R should be well-formed" (wff pred_sig exp_sig p_or);;

(* Test 4: Invalid predicates *)
let p_invalid1 = Pred (("X", 1), [| V "x" |]);;
assert_false "Unknown predicate symbol should fail" (wff pred_sig exp_sig p_invalid1);;

let p_invalid2 = Pred (("P", 1), [||]);;
assert_false "Wrong arity (P with 0 args) should fail" (wff pred_sig exp_sig p_invalid2);;

let p_invalid3 = Pred (("Q", 2), [| V "x" |]);;
assert_false "Wrong arity (Q with 1 arg) should fail" (wff pred_sig exp_sig p_invalid3);;

(* Test 5: Predicate substitution *)
let p_subst1 = Pred (("P", 1), [| V "x" |]);;
let subst_p1 = [("x", Node (("a", 0), [||]))];;
let result_p1 = psubst subst_p1 p_subst1;;
let expected_p1 = Pred (("P", 1), [| Node (("a", 0), [||]) |]);;
assert_true "Substitution in P(x) with x -> a()" (pred_equal result_p1 expected_p1);;

(* Test 6: Substitution in compound predicates *)
let p_subst2 = And (Pred (("P", 1), [| V "x" |]), Pred (("Q", 2), [| V "x"; V "y" |]));;
let subst_p2 = [("x", Node (("g", 1), [| V "z" |])); ("y", Node (("a", 0), [||]))];;
let result_p2 = psubst subst_p2 p_subst2;;
let expected_p2 = And (
  Pred (("P", 1), [| Node (("g", 1), [| V "z" |]) |]),
  Pred (("Q", 2), [| Node (("g", 1), [| V "z" |]); Node (("a", 0), [||]) |])
);;
assert_true "Substitution in P(x) ∧ Q(x, y)" (pred_equal result_p2 expected_p2);;

(* Test 7: Weakest Precondition *)
let p_wp = Pred (("Q", 2), [| V "x"; Node (("f", 2), [| V "x"; V "y" |]) |]);;
let e_wp = Node (("g", 1), [| V "z" |]);;
let result_wp = wp "x" e_wp p_wp;;
let expected_wp = Pred (("Q", 2), [| Node (("g", 1), [| V "z" |]); Node (("f", 2), [| Node (("g", 1), [| V "z" |]); V "y" |]) |]);;
assert_true "wp(x, g(z)) should replace all x occurrences" (pred_equal result_wp expected_wp);;

(* Test 8: Complex nested predicate *)
let p_complex_nested = And (
  Or (Pred (("P", 1), [| V "x" |]), Pred (("P", 1), [| V "y" |])),
  Not (Pred (("Q", 2), [| V "x"; V "y" |]))
);;
assert_true "Complex nested predicate should be well-formed" (wff pred_sig exp_sig p_complex_nested);;

let subst_complex = [("x", Node (("a", 0), [||]))];;
let result_complex = psubst subst_complex p_complex_nested;;
let expected_complex = And (
  Or (Pred (("P", 1), [| Node (("a", 0), [||]) |]), Pred (("P", 1), [| V "y" |])),
  Not (Pred (("Q", 2), [| Node (("a", 0), [||]); V "y" |]))
);;
assert_true "Substitution in complex nested predicate" (pred_equal result_complex expected_complex);;

Printf.printf "\n";;

(* Print final summary *)
print_summary();;
