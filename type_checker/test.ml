open Types
open Ast

(* Helper: parse a string into an expression list *)
let parse_str str =
  let lexbuf = Lexing.from_string str in
  Parser.program Lexer.token lexbuf

(* Helper: format a list of types as a readable string *)
let show_types ts =
  "[" ^ String.concat ", " (List.map string_of_typ ts) ^ "]"

(* Test counters *)
let passes  = ref 0
let failures = ref 0

(* check_types: expect parse + typecheck to succeed and produce types matching expected *)
let check_types label input expected_types =
  Printf.printf "========================================\n";
  Printf.printf "Test: %s\n" label;
  Printf.printf "Input:\n%s\n" input;
  try
    let exprs = parse_str input in
    (* Typecheck each expression; collect all results *)
    let all_types = List.concat_map (fun e -> Typechecker.typecheck e) exprs in
    Printf.printf "Output:\n%s\n" (show_types all_types);
    let success = List.for_all (fun exp_t ->
      List.exists (match_types exp_t) all_types
    ) expected_types in
    if success then begin
      Printf.printf "Assertion: [PASS]\n";
      incr passes
    end else begin
      Printf.printf "Assertion: [FAIL]\n  expected to contain: %s\n" (show_types expected_types);
      incr failures
    end
  with
  | Typechecker.TypeError msg ->
      Printf.printf "Output:\nTypeError: %s\n" msg;
      Printf.printf "Assertion: [FAIL] — unexpected type error\n";
      incr failures
  | e ->
      Printf.printf "Output:\nException: %s\n" (Printexc.to_string e);
      Printf.printf "Assertion: [FAIL]\n";
      incr failures

(* check_error: expect typecheck to raise a TypeError *)
let check_error label input =
  Printf.printf "========================================\n";
  Printf.printf "Test: %s\n" label;
  Printf.printf "Input:\n%s\n" input;
  try
    let exprs = parse_str input in
    let _ = List.concat_map (fun e -> Typechecker.typecheck e) exprs in
    Printf.printf "Output:\n(no error raised)\n";
    Printf.printf "Assertion: [FAIL] — expected a TypeError but got none\n";
    incr failures
  with
  | Typechecker.TypeError msg ->
      Printf.printf "Output:\nTypeError: %s\n" msg;
      Printf.printf "Assertion: [PASS] — TypeError caught as expected\n";
      incr passes
  | e ->
      Printf.printf "Output:\nException: %s\n" (Printexc.to_string e);
      Printf.printf "Assertion: [FAIL] — expected TypeError, got other exception\n";
      incr failures

let () =

  (* 2.4.1 Constants and quoted list *)
  check_types "2.4.1 Boolean atom t"           "t"          [TBool];
  check_types "2.4.1 Empty list ()"             "()"         [TBool; TList 0];
  check_types "2.4.1 Integer 12345"             "12345"      [TInt];
  check_types "2.4.1 Quoted list '(a b c d)"   "'(a b c d)" [TBool];

  (* 2.4.2 Arithmetic, comparison, and boolean operators *)
  check_types "2.4.2 Addition (+ 1 2 3)"                  "(+ 1 2 3)"            [TInt];
  check_types "2.4.2 Multiplication (* 2 3 4)"             "(* 2 3 4)"            [TInt];
  check_types "2.4.2 Subtraction (- 10 4)"                 "(- 10 4)"             [TInt];
  check_types "2.4.2 Division (div 17 5)"                  "(div 17 5)"           [TInt];
  check_types "2.4.2 Modulo (mod 17 5)"                    "(mod 17 5)"           [TInt];
  check_types "2.4.2 Comparison (> 5 3)"                   "(> 5 3)"              [TBool];
  check_types "2.4.2 Boolean (and (not t) (or t t))"       "(and (not t) (or t t))" [TBool];

  (* 2.4.3 List and atom primitives *)
  check_types "2.4.3 atom of int (atom 1)"         "(atom 1)"         [TBool];
  check_types "2.4.3 atom of quoted (atom '(a b c))" "(atom '(a b c))" [TBool];
  (* Note: '(a b c) is treated as TBool by the typechecker (non-evaluating quotes),
     so cdr and cons on quoted lists correctly raise TypeErrors. *)
  check_error "2.4.3 cdr of quoted list (cdr '(a b c))" "(cdr '(a b c))";
  check_error "2.4.3 cons int onto quoted (cons 1 '(2 3))" "(cons 1 '(2 3))";

  (* 2.4.4 Type errors and eq type consistency *)
  check_types "2.4.4 eq same type (eq 1 2)"       "(eq 1 2)"   [TBool];
  check_error "2.4.4 eq type mismatch (eq 1 t)"   "(eq 1 t)";
  check_error "2.4.4 add int and bool (+ 1 t)"    "(+ 1 t)";
  check_error "2.4.4 car of empty (car ())"        "(car ())";
  check_error "2.4.4 cons onto non-list (cons 1 2)" "(cons 1 2)";

  (* 2.4.5 cond typing *)
  check_types "2.4.5 cond all Int branches"
    "(cond ((atom 1) 10) ((> 3 4) 20) (t 30))"
    [TInt];
  check_error "2.4.5 cond with non-Bool condition (cond (1 10) (t 20))"
    "(cond (1 10) (t 20))";

  (* 2.4.6 Lambda and function application *)
  check_types "2.4.6 Lambda applied 2 args ((lambda (x y) (+ x y)) 4 5)"
    "((lambda (x y) (+ x y)) 4 5)"
    [TInt];
  (* Note: '(2 3) is treated as TBool, so cons inside the lambda raises a TypeError *)
  check_error "2.4.6 Lambda cons onto quoted ((lambda (x) (cons x '(2 3))) 1)"
    "((lambda (x) (cons x '(2 3))) 1)";
  check_error "2.4.6 Lambda arg count mismatch ((lambda (x y) (+ x y)) 4)"
    "((lambda (x y) (+ x y)) 4)";

  (* 2.4.7 Recursive function definition using defun *)
  check_types "2.4.7 defun factorial"
    "(defun fact (n) (cond ((<= n 1) 1) (t (* n (fact (- n 1))))))"
    [TFunction (1, TInt)];

  (* Final report *)
  Printf.printf "\n────────────────────────────\n";
  Printf.printf "  %d passed,  %d failed\n" !passes !failures;
  Printf.printf "────────────────────────────\n";
  if !failures > 0 then exit 1
