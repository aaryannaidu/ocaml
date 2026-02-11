(* Load the main file *)
#use "main.ml";;

open Signature;;
open Expression;;
open Substitution;;

Printf.printf "=== TESTING SUBSTITUTION MODULE ===\n\n";;

(* Define a test signature *)
let test_sig = [("f", 2); ("g", 1); ("a", 0); ("h", 3)];;

(* Helper function to print expressions *)
let rec exp_to_string exp =
  match exp with
  | V var -> var
  | Node ((name, _), args) ->
      if Array.length args = 0 then
        name ^ "()"
      else
        name ^ "(" ^ String.concat ", " (Array.to_list (Array.map exp_to_string args)) ^ ")"
;;

(* Test 1: Simple substitution *)
Printf.printf "=== Test 1: Simple Substitution ===\n";;
let exp1 = V "x";;
let s1 = [("x", Node (("a", 0), [||]))];;
let result1 = subst s1 exp1;;
Printf.printf "exp: %s\n" (exp_to_string exp1);;
Printf.printf "subst: x -> a()\n";;
Printf.printf "result: %s\n\n" (exp_to_string result1);;

(* Test 2: Substitution in nested expression *)
Printf.printf "=== Test 2: Nested Substitution ===\n";;
let exp2 = Node (("f", 2), [| V "x"; V "y" |]);;
let s2 = [("x", Node (("g", 1), [| V "z" |])); ("y", Node (("a", 0), [||]))];;
let result2 = subst s2 exp2;;
Printf.printf "exp: %s\n" (exp_to_string exp2);;
Printf.printf "subst: x -> g(z), y -> a()\n";;
Printf.printf "result: %s\n\n" (exp_to_string result2);;

(* Test 3: Partial substitution *)
Printf.printf "=== Test 3: Partial Substitution ===\n";;
let exp3 = Node (("f", 2), [| V "x"; V "y" |]);;
let s3 = [("x", Node (("a", 0), [||]))];;
let result3 = subst s3 exp3;;
Printf.printf "exp: %s\n" (exp_to_string exp3);;
Printf.printf "subst: x -> a() (y not substituted)\n";;
Printf.printf "result: %s\n\n" (exp_to_string result3);;

(* Test 4: No substitution *)
Printf.printf "=== Test 4: No Substitution ===\n";;
let exp4 = Node (("f", 2), [| V "x"; V "y" |]);;
let s4 = [];;
let result4 = subst s4 exp4;;
Printf.printf "exp: %s\n" (exp_to_string exp4);;
Printf.printf "subst: empty\n";;
Printf.printf "result: %s\n\n" (exp_to_string result4);;

(* Test 5: Composition of substitutions *)
Printf.printf "=== Test 5: Composition ===\n";;
let s5a = [("x", V "y"); ("z", Node (("a", 0), [||]))];;
let s5b = [("y", Node (("g", 1), [| V "w" |]))];;
let comp = compose s5a s5b;;
Printf.printf "s1: x -> y, z -> a()\n";;
Printf.printf "s2: y -> g(w)\n";;
Printf.printf "composition (s1 o s2):\n";;
List.iter (fun (var, exp) -> 
  Printf.printf "  %s -> %s\n" var (exp_to_string exp)
) comp;;
Printf.printf "\n";;

(* Test 6: Edit at position *)
Printf.printf "=== Test 6: Edit at Position ===\n";;
let exp6 = Node (("f", 2), [| Node (("g", 1), [| V "x" |]); Node (("a", 0), [||]) |]);;
Printf.printf "original: %s\n" (exp_to_string exp6);;

(* Edit position [0, 0] - replace the "x" inside g(...) *)
let new_exp6 = Node (("a", 0), [||]);;
match edit [0; 0] new_exp6 exp6 with
| Some result -> Printf.printf "edit [0,0] with a(): %s\n" (exp_to_string result)
| None -> Printf.printf "edit [0,0]: invalid position\n";;

(* Edit position [1] - replace second argument *)
match edit [1] (V "z") exp6 with
| Some result -> Printf.printf "edit [1] with z: %s\n" (exp_to_string result)
| None -> Printf.printf "edit [1]: invalid position\n";;

(* Invalid edit - position too deep *)
match edit [0; 0; 0] new_exp6 exp6 with
| Some result -> Printf.printf "edit [0,0,0]: %s\n" (exp_to_string result)
| None -> Printf.printf "edit [0,0,0]: invalid position (can't descend into variable)\n";;
Printf.printf "\n";;

(* Test 7: In-place substitution *)
Printf.printf "=== Test 7: In-place Substitution ===\n";;
let exp7 = Node (("f", 2), [| V "x"; Node (("g", 1), [| V "y" |]) |]);;
Printf.printf "before: %s\n" (exp_to_string exp7);;
let s7 = [("x", Node (("a", 0), [||])); ("y", V "z")];;
subst_inplace s7 exp7;;
Printf.printf "after in-place subst (x->a(), y->z): %s\n" (exp_to_string exp7);;
Printf.printf "\n";;

(* Test 8: Deep substitution *)
Printf.printf "=== Test 8: Deep Substitution ===\n";;
let exp8 = Node (("h", 3), [| 
  Node (("f", 2), [| V "x"; V "y" |]); 
  V "z";
  Node (("g", 1), [| V "x" |])
|]);;
let s8 = [("x", Node (("a", 0), [||]))];;
let result8 = subst s8 exp8;;
Printf.printf "exp: %s\n" (exp_to_string exp8);;
Printf.printf "subst: x -> a()\n";;
Printf.printf "result: %s\n" (exp_to_string result8);;
Printf.printf "Notice that both occurrences of 'x' were replaced!\n";;
