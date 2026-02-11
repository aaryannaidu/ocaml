(* Load the main file *)
#use "main.ml";;

open Signature;;
open Expression;;

(* ========================================
   TEST PART 1: SIGNATURES
   ======================================== *)

Printf.printf "=== TESTING SIGNATURES ===\n";;

(* Test 1: Valid signature *)
let sig1 = [("f", 2); ("g", 1); ("a", 0)];;
Printf.printf "Test 1 (valid signature): %b\n" (check_sig sig1);;

(* Test 2: Duplicate symbols *)
let sig2 = [("f", 2); ("g", 1); ("f", 0)];;
Printf.printf "Test 2 (duplicate 'f'): %b\n" (check_sig sig2);;

(* Test 3: Negative arity *)
let sig3 = [("f", 2); ("g", -1); ("a", 0)];;
Printf.printf "Test 3 (negative arity): %b\n" (check_sig sig3);;

(* Test 4: Empty signature *)
let sig4 = [];;
Printf.printf "Test 4 (empty signature): %b\n" (check_sig sig4);;

(* Test 5: Single symbol *)
let sig5 = [("f", 3)];;
Printf.printf "Test 5 (single symbol): %b\n\n" (check_sig sig5);;


(* ========================================
   TEST PART 2: EXPRESSIONS
   ======================================== *)

Printf.printf "=== TESTING EXPRESSIONS ===\n";;

(* Define a valid signature for testing *)
let test_sig = [("f", 2); ("g", 1); ("a", 0); ("h", 3)];;

(* Test expressions *)
(* exp1: x (just a variable) *)
let exp1 = V "x";;
Printf.printf "exp1 = V \"x\"\n";;
Printf.printf "  wfexp: %b, ht: %d, size: %d, vars: [%s]\n\n" 
  (wfexp test_sig exp1) 
  (ht exp1) 
  (size exp1)
  (String.concat "; " (vars exp1));;

(* exp2: a() - nullary symbol *)
let exp2 = Node (("a", 0), [||]);;
Printf.printf "exp2 = Node ((\"a\", 0), [||])\n";;
Printf.printf "  wfexp: %b, ht: %d, size: %d, vars: [%s]\n\n" 
  (wfexp test_sig exp2) 
  (ht exp2) 
  (size exp2)
  (String.concat "; " (vars exp2));;

(* exp3: g(x) - unary function *)
let exp3 = Node (("g", 1), [| V "x" |]);;
Printf.printf "exp3 = Node ((\"g\", 1), [| V \"x\" |])\n";;
Printf.printf "  wfexp: %b, ht: %d, size: %d, vars: [%s]\n\n" 
  (wfexp test_sig exp3) 
  (ht exp3) 
  (size exp3)
  (String.concat "; " (vars exp3));;

(* exp4: f(x, y) - binary function *)
let exp4 = Node (("f", 2), [| V "x"; V "y" |]);;
Printf.printf "exp4 = Node ((\"f\", 2), [| V \"x\"; V \"y\" |])\n";;
Printf.printf "  wfexp: %b, ht: %d, size: %d, vars: [%s]\n\n" 
  (wfexp test_sig exp4) 
  (ht exp4) 
  (size exp4)
  (String.concat "; " (vars exp4));;

(* exp5: f(g(x), a()) - nested expression *)
let exp5 = Node (("f", 2), [| Node (("g", 1), [| V "x" |]); Node (("a", 0), [||]) |]);;
Printf.printf "exp5 = Node ((\"f\", 2), [| Node ((\"g\", 1), [| V \"x\" |]); Node ((\"a\", 0), [||]) |])\n";;
Printf.printf "  wfexp: %b, ht: %d, size: %d, vars: [%s]\n\n" 
  (wfexp test_sig exp5) 
  (ht exp5) 
  (size exp5)
  (String.concat "; " (vars exp5));;

(* exp6: h(x, y, z) - ternary function *)
let exp6 = Node (("h", 3), [| V "x"; V "y"; V "z" |]);;
Printf.printf "exp6 = Node ((\"h\", 3), [| V \"x\"; V \"y\"; V \"z\" |])\n";;
Printf.printf "  wfexp: %b, ht: %d, size: %d, vars: [%s]\n\n" 
  (wfexp test_sig exp6) 
  (ht exp6) 
  (size exp6)
  (String.concat "; " (vars exp6));;

(* exp7: f(f(x, y), g(z)) - more complex nesting *)
let exp7 = Node (("f", 2), [| 
  Node (("f", 2), [| V "x"; V "y" |]); 
  Node (("g", 1), [| V "z" |]) 
|]);;
Printf.printf "exp7 = Node ((\"f\", 2), [| Node ((\"f\", 2), [| V \"x\"; V \"y\" |]); Node ((\"g\", 1), [| V \"z\" |]) |])\n";;
Printf.printf "  wfexp: %b, ht: %d, size: %d, vars: [%s]\n\n" 
  (wfexp test_sig exp7) 
  (ht exp7) 
  (size exp7)
  (String.concat "; " (vars exp7));;

(* Test invalid expressions *)
Printf.printf "=== TESTING INVALID EXPRESSIONS ===\n";;

(* exp8: f(x) - wrong arity (f expects 2 args) *)
let exp8 = Node (("f", 2), [| V "x" |]);;
Printf.printf "exp8 = Node ((\"f\", 2), [| V \"x\" |]) - wrong arity\n";;
Printf.printf "  wfexp: %b\n\n" (wfexp test_sig exp8);;

(* exp9: k(x) - symbol not in signature *)
let exp9 = Node (("k", 1), [| V "x" |]);;
Printf.printf "exp9 = Node ((\"k\", 1), [| V \"x\" |]) - symbol not in sig\n";;
Printf.printf "  wfexp: %b\n\n" (wfexp test_sig exp9);;

(* exp10: Variable with duplicate occurrences *)
let exp10 = Node (("f", 2), [| V "x"; V "x" |]);;
Printf.printf "exp10 = Node ((\"f\", 2), [| V \"x\"; V \"x\" |]) - duplicate var\n";;
Printf.printf "  wfexp: %b, vars: [%s]\n" 
  (wfexp test_sig exp10)
  (String.concat "; " (vars exp10));;
