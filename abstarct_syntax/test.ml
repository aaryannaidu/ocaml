(* Load the main file *)
#use "main.ml";;

open Signature;;
open Expression;;
open Substitution;;
open Predicates;;

(* ========================================
   OUTPUT SETUP — redirect to output.txt
   ======================================== *)

let oc = open_out "output.txt";;
let print s = output_string oc s; output_string oc "\n"; flush oc;;
let printf fmt = Printf.ksprintf print fmt;;

(* ========================================
   PRETTY PRINTERS
   ======================================== *)

let rec string_of_exp e =
  match e with
  | V x -> x
  | Node ((name, _), args) ->
      if Array.length args = 0 then name
      else name ^ "(" ^ (String.concat ", " (Array.to_list (Array.map string_of_exp args))) ^ ")";;

let rec string_of_pred p =
  match p with
  | T -> "T"
  | F -> "F"
  | Pred ((name, _), args) ->
      if Array.length args = 0 then name
      else name ^ "(" ^ (String.concat ", " (Array.to_list (Array.map string_of_exp args))) ^ ")"
  | Not p' -> "NOT(" ^ string_of_pred p' ^ ")"
  | And (p1, p2) -> "AND(" ^ string_of_pred p1 ^ ", " ^ string_of_pred p2 ^ ")"
  | Or (p1, p2) -> "OR(" ^ string_of_pred p1 ^ ", " ^ string_of_pred p2 ^ ")";;

(* ========================================
   PART 1: check_sig
   ======================================== *)

printf "============================================================";;
printf "PART 1: check_sig";;
printf "============================================================";;

(* 1. check_sig [] *)
printf "";;
printf "1. check_sig []";;
printf "   Result: %b" (check_sig []);;

(* 2. check_sig [("f", 2); ("g", 1); ("a", 0)] *)
printf "";;
printf "2. check_sig [(\"f\", 2); (\"g\", 1); (\"a\", 0)]";;
printf "   Result: %b" (check_sig [("f", 2); ("g", 1); ("a", 0)]);;

(* 3. check_sig [("f", 2); ("g", -3)]  — negative arity *)
printf "";;
printf "3. check_sig [(\"f\", 2); (\"g\", -3)]";;
printf "   Result: %b" (check_sig [("f", 2); ("g", -3)]);;

(* 4. check_sig [("f", 2); ("b", 1); ("f", 1)]  — duplicate *)
printf "";;
printf "4. check_sig [(\"f\", 2); (\"b\", 1); (\"f\", 1)]";;
printf "   Result: %b" (check_sig [("f", 2); ("b", 1); ("f", 1)]);;

(* 5. check_sig [("f", 0); ("g", 0)] *)
printf "";;
printf "5. check_sig [(\"f\", 0); (\"g\", 0)]";;
printf "   Result: %b" (check_sig [("f", 0); ("g", 0)]);;

(* 6. check_sig [("f", 1000)] *)
printf "";;
printf "6. check_sig [(\"f\", 1000)]";;
printf "   Result: %b" (check_sig [("f", 1000)]);;

(* ========================================
   PART 2A: wfexp
   Given Signature: S = [("f", 2); ("g", 1); ("a", 0)]
   ======================================== *)

printf "";;
printf "============================================================";;
printf "PART 2A: wfexp";;
printf "Given Signature: S = [(\"f\", 2); (\"g\", 1); (\"a\", 0)]";;
printf "============================================================";;

let s_2a = [("f", 2); ("g", 1); ("a", 0)];;

(* 1. wfexp (Node ("f", [| Node ("g", [| V "x" |]); Node ("a", [||]) |])) *)
let wf1 = Node (("f", 2), [| Node (("g", 1), [| V "x" |]); Node (("a", 0), [||]) |]);;
printf "";;
printf "1. wfexp (Node (\"f\", [| Node (\"g\", [| V \"x\" |]); Node (\"a\", [||]) |]))";;
printf "   Result: %b" (wfexp s_2a wf1);;

(* 2. wfexp (Node ("f", [| V "x" |]))  — arity mismatch *)
let wf2 = Node (("f", 2), [| V "x" |]);;
printf "";;
printf "2. wfexp (Node (\"f\", [| V \"x\" |]))   (* arity mismatch: f has arity 2 *)";;
printf "   Result: %b" (wfexp s_2a wf2);;

(* 3. wfexp (Node ("f", [| V "x"; V "y"; V "z" |]))  — arity mismatch *)
let wf3 = Node (("f", 2), [| V "x"; V "y"; V "z" |]);;
printf "";;
printf "3. wfexp (Node (\"f\", [| V \"x\"; V \"y\"; V \"z\" |]))   (* arity mismatch *)";;
printf "   Result: %b" (wfexp s_2a wf3);;

(* 4. wfexp (Node ("h", [| V "x" |]))  — h not in signature *)
let wf4 = Node (("h", 1), [| V "x" |]);;
printf "";;
printf "4. wfexp (Node (\"h\", [| V \"x\" |]))   (* h not in signature *)";;
printf "   Result: %b" (wfexp s_2a wf4);;

(* 5. wfexp (Node ("f", [| Node ("a", [| V "x" |]); V "y" |]))  — a has arity 0 but given 1 arg *)
let wf5 = Node (("f", 2), [| Node (("a", 0), [| V "x" |]); V "y" |]);;
printf "";;
printf "5. wfexp (Node (\"f\", [| Node (\"a\", [| V \"x\" |]); V \"y\" |]))   (* a has arity 0, given 1 arg *)";;
printf "   Result: %b" (wfexp s_2a wf5);;

(* ========================================
   PART 2B: ht, size, vars
   ======================================== *)

printf "";;
printf "============================================================";;
printf "PART 2B: ht, size, vars";;
printf "============================================================";;

(* All three on: Node ("f", [| Node ("g", [| V "x" |]); Node ("a", [||]) |]) *)
let e_2b = Node (("f", 2), [| Node (("g", 1), [| V "x" |]); Node (("a", 0), [||]) |]);;

printf "";;
printf "Expression: Node (\"f\", [| Node (\"g\", [| V \"x\" |]); Node (\"a\", [||]) |])";;
printf "";;
printf "1. ht  = %d" (ht e_2b);;

(* size of: Node ("f", [| Node ("g", [| V "x" |]); Node ("a", [||]) |]) *)
let e_2b_size = Node (("f", 2), [| Node (("g", 1), [| V "x" |]); Node (("a", 0), [||]) |]);;
printf "2. size = %d" (size e_2b_size);;

(* vars of: Node ("f", [| Node ("g", [| V "y" |]); V "x" |]) *)
let e_2b_vars = Node (("f", 2), [| Node (("g", 1), [| V "y" |]); V "x" |]);;
printf "";;
printf "Expression for vars: Node (\"f\", [| Node (\"g\", [| V \"y\" |]); V \"x\" |])";;
printf "3. vars = [%s]" (String.concat "; " (vars e_2b_vars));;

(* ========================================
   PART 3: subst, compose, edit, inplace_subst
   Given sigma = [("fn",3); ("f",2); ("g",1); ("h",2); ("a",0); ("b",0); ("c",0)]
   ======================================== *)

printf "";;
printf "============================================================";;
printf "PART 3: subst, compose, edit, inplace_subst";;
printf "sigma = [(\"fn\",3); (\"f\",2); (\"g\",1); (\"h\",2); (\"a\",0); (\"b\",0); (\"c\",0)]";;
printf "============================================================";;

(* Common atoms *)
let x_v = V "x";;
let y_v = V "y";;
let z_v = V "z";;
let a_n = Node (("a", 0), [||]);;
let b_n = Node (("b", 0), [||]);;
let c_n = Node (("c", 0), [||]);;

(* --- 1. Substitution --- *)
printf "";;
printf "--- 1. Substitution ---";;
printf "s1  = [(\"x\", Node(\"h\",[|b; y|])); (\"y\", Node(\"g\",[|a|]))]";;
printf "e1  = Node(\"fn\",[|Node(\"g\",[|x|]); Node(\"h\",[|x; y|]); z|])";;

let s1_subst = [
  ("x", Node (("h", 2), [| b_n; y_v |]));
  ("y", Node (("g", 1), [| a_n |]))
];;
let e1 = Node (("fn", 3), [|
  Node (("g", 1), [| x_v |]);
  Node (("h", 2), [| x_v; y_v |]);
  z_v
|]);;
let result_subst = subst s1_subst e1;;
printf "subst e1 s1 = %s" (string_of_exp result_subst);;

(* --- 2. Composition --- *)
printf "";;
printf "--- 2. Composition ---";;
printf "s1_comp = [(\"x\", Node(\"g\",[|y|]))]";;
printf "s2_comp = [(\"x\", b); (\"y\", Node(\"h\",[|a; b|])); (\"z\", a)]";;
printf "e2      = Node(\"h\",[|x; z|])";;

let s1_comp = [("x", Node (("g", 1), [| y_v |]))];;
let s2_comp = [
  ("x", b_n);
  ("y", Node (("h", 2), [| a_n; b_n |]));
  ("z", a_n)
];;
let e2 = Node (("h", 2), [| x_v; z_v |]);;
let composed = compose s1_comp s2_comp;;
let result_comp = subst composed e2;;
printf "subst e2 (compose s1_comp s2_comp) = %s" (string_of_exp result_comp);;

(* --- 3. Edit by position --- *)
printf "";;
printf "--- 3. Edit by position ---";;
printf "e3 = Node(\"fn\",[|Node(\"g\",[|x|]); c; Node(\"h\",[|b; a|])|])";;
printf "edit e3 [2;1] (Node(\"g\",[|c|]))";;
(* Position [2;1] means: go to child index 2, then child index 1
   Note: testcase uses 1-indexed positions; our implementation uses 0-indexed.
   The testcase says [2;1] which in 1-indexed means 3rd child then 2nd child.
   Our edit uses 0-indexed, so that would be [2;1] -> same numeric values but 0-indexed.
   Let's test both interpretations and show both. *)

let e3 = Node (("fn", 3), [|
  Node (("g", 1), [| x_v |]);
  c_n;
  Node (("h", 2), [| b_n; a_n |])
|]);;
let new_node = Node (("g", 1), [| c_n |]);;

(* 0-indexed: [2;1] = e3's 3rd child (Node "h"), then its 2nd child (a) *)
(let pos = [2; 1] in
 match edit pos new_node e3 with
 | Some result -> printf "   Result (0-indexed pos [2;1]): %s" (string_of_exp result)
 | None -> printf "   Result (0-indexed pos [2;1]): invalid position");;

(* --- 4. In-place substitution --- *)
printf "";;
printf "--- 4. In-place substitution ---";;
printf "e4 = Node(\"f\",[|x; Node(\"g\",[|x|])|])";;
printf "s4 = [(\"x\", Node(\"g\",[|b|]))]";;

let e4 = Node (("f", 2), [| x_v; Node (("g", 1), [| x_v |]) |]);;
let s4 = [("x", Node (("g", 1), [| b_n |]))];;
subst_inplace s4 e4;;
printf "inplace_subst e4 s4 => e4 is now: %s" (string_of_exp e4);;

(* ========================================
   PART 5: wff, psubst, wp
   pi = [("P",2); ("Q",1); ("R",0)]
   ======================================== *)

printf "";;
printf "============================================================";;
printf "PART 5: wff, psubst, wp";;
printf "pi = [(\"P\",2); (\"Q\",1); (\"R\",0)]";;
printf "============================================================";;

let pi = [("P", 2); ("Q", 1); ("R", 0)];;
let exp_sig_5 = [("a", 0); ("b", 0); ("h", 2); ("g", 1)];;

let a5 = Node (("a", 0), [||]);;
let b5 = Node (("b", 0), [||]);;
let x5 = V "x";;
let y5 = V "y";;

(* --- 1. wff checks --- *)
printf "";;
printf "--- 1. wff checks ---";;

printf "wff pi T         = %b" (wff pi exp_sig_5 T);;
printf "wff pi F         = %b" (wff pi exp_sig_5 F);;

(* wff pi (Pred("R",[|a|]))  — R has arity 0, so no args expected *)
printf "wff pi (Pred(\"R\",[|a|]))  (* R has arity 0, given 1 arg => false *) = %b"
  (wff pi exp_sig_5 (Pred (("R", 0), [| a5 |])));;

printf "wff pi (Pred(\"R\",[||]))   (* R with 0 args => true *) = %b"
  (wff pi exp_sig_5 (Pred (("R", 0), [||])));;

(* --- 2. Predicate substitution --- *)
printf "";;
printf "--- 2. Predicate substitution ---";;
printf "s_p = [(\"x\", Node(\"h\",[|b; y|])); (\"y\", Node(\"g\",[|a|]))]";;
printf "p_p = Or(Pred(\"Q\",[|x|]), Pred(\"P\",[|x; y|]))";;

let s_p = [
  ("x", Node (("h", 2), [| b5; y5 |]));
  ("y", Node (("g", 1), [| a5 |]))
];;
let p_p = Or (
  Pred (("Q", 1), [| x5 |]),
  Pred (("P", 2), [| x5; y5 |])
);;
let result_psubst = psubst s_p p_p;;
printf "psubst p_p s_p = %s" (string_of_pred result_psubst);;

(* --- 3. Weakest precondition --- *)
printf "";;
printf "--- 3. Weakest precondition ---";;
printf "p_wp = And(Not(Pred(\"P\",[|x; y|])), Pred(\"Q\",[|x|]))";;
printf "wp \"x\" (Node(\"g\",[|b|])) p_wp";;

let p_wp = And (
  Not (Pred (("P", 2), [| x5; y5 |])),
  Pred (("Q", 1), [| x5 |])
);;
let result_wp = wp "x" (Node (("g", 1), [| b5 |])) p_wp;;
printf "wp result = %s" (string_of_pred result_wp);;

(* ========================================
   DONE
   ======================================== *)
printf "";;
printf "============================================================";;
printf "All test cases completed. Results written to output.txt";;
printf "============================================================";;

close_out oc;;
