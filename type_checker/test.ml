open Types
open Ast

(* A helper to parse a string into an expression list *)
let parse_str str =
  let lexbuf = Lexing.from_string str in
  Parser.program Lexer.token lexbuf

(* Helper to check if a code sample evaluates to the expected types *)
let assert_types code expected_types =
  try
    let exprs = parse_str code in
    List.iter (fun e ->
      let inferred = Typechecker.typecheck e in
      
      (* Check if all expected types are present in inferred types *)
      let success = List.for_all (fun exp_t -> 
         List.exists (match_types exp_t) inferred
      ) expected_types in
      
      if success then
        Printf.printf "[PASS] %-30s -> [%s]\n" code (String.concat ", " (List.map string_of_typ inferred))
      else begin
        Printf.printf "[FAIL] %-30s\n" code;
        Printf.printf "       Expected: [%s]\n" (String.concat ", " (List.map string_of_typ expected_types));
        Printf.printf "       Got     : [%s]\n" (String.concat ", " (List.map string_of_typ inferred));
      end
    ) exprs
  with
  | Typechecker.TypeError msg ->
      Printf.printf "[FAIL] %-30s -> Raised Unexpected Type Error: %s\n" code msg
  | e ->
      Printf.printf "[ERROR] %-30s -> Parsing/Lexing Failed: %s\n" code (Printexc.to_string e)

(* Helper to test that a piece of code strictly FAILS type checking *)
let assert_type_error code =
  try
    let exprs = parse_str code in
    List.iter (fun e ->
      let _ = Typechecker.typecheck e in
      Printf.printf "[FAIL] %-30s -> Expected a TypeError, but it passed!\n" code
    ) exprs
  with
  | Typechecker.TypeError msg ->
      Printf.printf "[PASS] %-30s -> (Caught expected error)\n" code
  | e ->
      Printf.printf "[ERROR] %-30s -> Parsing/Lexing Failed instead of Type Error\n" code

let () =
  print_endline "=== LITHP Type Checker Component Tests ===\n";
  
  print_endline "--- 1. Basic Types ---";
  assert_types "5" [TInt];
  assert_types "'t" [TBool];
  assert_types "t" [TBool];
  
  print_endline "\n--- 2. Overloading the Empty List () ---";
  assert_types "()" [TBool; TList 0];

  print_endline "\n--- 3. Arithmetic & Logic ---";
  assert_types "(+ 1 2 3)" [TInt];
  assert_types "(* 4 5)" [TInt];
  assert_types "(- 10 5)" [TInt];
  assert_types "(> 5 3)" [TBool];
  assert_types "(not 't)" [TBool];
  assert_types "(and 't ())" [TBool]; (* () can be boolean *)

  print_endline "\n--- 4. List Operations ---";
  (* Creating a list of length 1 by consing onto empty list (List(0)) *)
  assert_types "(cons 1 ())" [TList 1];
  assert_types "(cons 'a (cons 'b ()))" [TList 2];
  
  (* car takes List(n>0) and returns Any *)
  assert_types "(car (cons 1 ()))" [TAny];
  
  (* cdr takes List(n>0) and returns List(n-1) *)
  assert_types "(cdr (cons 1 (cons 2 ())))" [TList 1];

  print_endline "\n--- 5. Cond statements ---";
  assert_types "(cond ((> 5 3) 1) ('t 2))" [TInt];
  
  print_endline "\n--- 6. Functions (Lambda & Defun) ---";
  (* Function taking 1 argument -> Int *)
  assert_types "(lambda (x) (+ x 1))" [TFunction (1, TInt)];
  (* Applying the function directly *)
  assert_types "((lambda (x) (+ x 1)) 5)" [TInt];
  
  (* Using defun *)
  assert_types "(defun addone (x) (+ x 1))" [TFunction (1, TInt)];

  print_endline "\n--- 7. Expecting Failures (Type Errors) ---";
  assert_type_error "(+ 1 't)";         (* Cannot add int to bool *)
  assert_type_error "(car ())";        (* Cannot car an empty list, n must be > 0 *)
  assert_type_error "(not 5)";         (* not expects bool *)
  assert_type_error "(cons 1 2)";      (* cons expects List(n) as second arg *)
  assert_type_error "(eq 5 't)";       (* eq expects both arguments to share a type *)
  assert_type_error "((lambda (x y) (+ x y)) 5)"; (* Incorrect number of arguments *)
  assert_type_error "(cond ((+ 1 2) 5))"; (* cond condition must be a bool *)

  print_endline "\n--- 8. Rigorous & Edge Cases (Success) ---";
  
  (* Nested Lambdas (Currying) *)
  assert_types "(((lambda (x) (lambda (y) (+ x y))) 5) 10)" [TInt];
  
  (* Overloading trick: () acting as Bool AND List(0) in the exact same expression! *)
  assert_types "(and () (eq (cdr (cons 1 ())) ()))" [TBool];
  
  (* label recursion: factorial function *)
  assert_types "(label fact (lambda (n) (cond ((eq n 0) 1) ('t (* n (fact (- n 1)))))))" [TFunction (1, TInt)];
  
  (* Complex defun returning lists *)
  assert_types "(defun buildlist (x) (cons x (cons (+ x 1) ())))" [TFunction (1, TList 2)];
  
  (* Nested functions and arguments *)
  assert_types "((lambda (f lst) (cons (f 5) lst)) (lambda (x) (+ x 10)) ())" [TList 1];

  print_endline "\n--- 9. Rigorous Edge Cases (Expecting Failures) ---";
  
  (* Cond branches do not share a common type (Strong typing enforcement!) *)
  assert_type_error "(cond ('t 1) (() 't))"; (* Int vs Bool *)
  
  (* Incorrect argument counts for built-ins *)
  assert_type_error "(+)";
  assert_type_error "(+ 1)";
  assert_type_error "(- 1 2 3)"; (* minus strictly takes 2 *)
  
  (* Attempting to car/cdr non-lists or empty lists *)
  assert_type_error "(car 5)";
  assert_type_error "(cdr 't)";
  
  (* Erroneous recursion: calling label function with wrong arity *)
  assert_type_error "(label f (lambda (x) (f x 1)))";

  print_endline "\n============================================\n"
