open Main

(* Helper to convert lam to string *)
let rec string_of_lam = function
  | V x -> x
  | Lam (x, m) -> "(\\" ^ x ^ ". " ^ string_of_lam m ^ ")"
  | App (m1, m2) -> "(" ^ string_of_lam m1 ^ " " ^ string_of_lam m2 ^ ")"

(* Helper to convert comb to string *)
let rec string_of_comb = function
  | Vc x -> x
  | S -> "S"
  | K -> "K"
  | I -> "I"
  | Appc (c1, c2) -> "(" ^ string_of_comb c1 ^ " " ^ string_of_comb c2 ^ ")"

(* Test runner for lambda terms *)
let assert_eval_lam name input expected_comb =
  let c = translate input in
  let result = wnf c [] in
  Printf.printf "Test: %s\n" name;
  Printf.printf "Input (lam):  %s\n" (string_of_lam input);
  Printf.printf "Trans (comb): %s\n" (string_of_comb c);
  Printf.printf "Eval (comb):  %s\n" (string_of_comb result);
  Printf.printf "Expected:     %s\n" (string_of_comb expected_comb);
  if result = expected_comb then
    Printf.printf "Result: PASSED\n\n"
  else begin
    Printf.printf "Result: FAILED\n\n";
    assert false
  end

(* Test runner for direct combinator terms *)
let assert_eval_comb name input expected_comb =
  let result = wnf input [] in
  Printf.printf "Test: %s\n" name;
  Printf.printf "Input (comb): %s\n" (string_of_comb input);
  Printf.printf "Eval (comb):  %s\n" (string_of_comb result);
  Printf.printf "Expected:     %s\n" (string_of_comb expected_comb);
  if result = expected_comb then
    Printf.printf "Result: PASSED\n\n"
  else begin
    Printf.printf "Result: FAILED\n\n";
    assert false
  end

let () =
  Printf.printf "--- Testing Basic Combinators ---\n\n";
  
  (* I x = x *)
  assert_eval_comb "I combinator" 
    (Appc (I, Vc "x")) 
    (Vc "x");
    
  (* K x y = x *)
  assert_eval_comb "K combinator" 
    (Appc (Appc (K, Vc "x"), Vc "y")) 
    (Vc "x");
    
  (* S K K x = x *)
  assert_eval_comb "S K K combinator (Identity)" 
    (Appc (Appc (Appc (S, K), K), Vc "x")) 
    (Vc "x");

  Printf.printf "--- Testing Lambda Translations ---\n\n";
  
  (* (\x. x) y -> y *)
  assert_eval_lam "Identity lambda"
    (App (Lam ("x", V "x"), V "y"))
    (Vc "y");
    
  (* (\x. \y. x) a b -> a *)
  let true_lam = Lam ("x", Lam ("y", V "x")) in
  assert_eval_lam "True boolean"
    (App (App (true_lam, V "a"), V "b"))
    (Vc "a");

  (* (\x. \y. y) a b -> b *)
  let false_lam = Lam ("x", Lam ("y", V "y")) in
  assert_eval_lam "False boolean"
    (App (App (false_lam, V "a"), V "b"))
    (Vc "b");

  Printf.printf "--- Testing Church Numerals ---\n\n";

  let zero = Lam ("f", Lam ("x", V "x")) in
  let one = Lam ("f", Lam ("x", App (V "f", V "x"))) in
  let two = Lam ("f", Lam ("x", App (V "f", App (V "f", V "x")))) in

  (* 0 f y -> y *)
  assert_eval_lam "Church Numeral 0"
    (App (App (zero, V "f"), V "y"))
    (Vc "y");

  (* 1 f y -> f y *)
  assert_eval_lam "Church Numeral 1"
    (App (App (one, V "f"), V "y"))
    (Appc (Vc "f", Vc "y"));

  (* 2 f y -> f (f y) *)
  assert_eval_lam "Church Numeral 2"
    (App (App (two, V "f"), V "y"))
    (Appc (Vc "f", Appc (Vc "f", Vc "y")));

  Printf.printf "All tests completed successfully!\n"
