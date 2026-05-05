(* Helper: parse a string, return Ast.exp list *)
let parse str =
  let lexbuf = Lexing.from_string str in
  Parser.program Lexer.token lexbuf

(* Helper: turn a list of exps into one printable string *)
let show_all exprs =
  String.concat "\n" (List.map Ast.show_exp exprs)

(* Test counters *)
let passes  = ref 0
let failures = ref 0

(* check label input expected_output *)
let check label input expected =
  Printf.printf "========================================\n";
  Printf.printf "Test: %s\n" label;
  Printf.printf "Input:\n%s\n" input;
  try
    let got = show_all (parse input) in
    Printf.printf "Output:\n%s\n" got;
    if got = expected then begin
      Printf.printf "Assertion: [PASS]\n";
      incr passes
    end else begin
      Printf.printf "Assertion: [FAIL]\n  expected: %s\n" expected;
      incr failures
    end
  with
  | Parsing.Parse_error ->
      Printf.printf "Output:\nParse error\n";
      if expected = "Parse error" then begin
        Printf.printf "Assertion: [PASS]\n";
        incr passes
      end else begin
        Printf.printf "Assertion: [FAIL]\n  expected: %s\n" expected;
        incr failures
      end
  | Failure msg ->
      Printf.printf "Output:\nLexer error: %s\n" msg;
      if expected = ("Lexer error: " ^ msg) then begin
        Printf.printf "Assertion: [PASS]\n";
        incr passes
      end else begin
        Printf.printf "Assertion: [FAIL]\n  expected: %s\n" expected;
        incr failures
      end

let () =
  (* 2.3.1 Atom, number, empty list, singleton list *)
  check "2.3.1 Atom" "foo" "foo";
  check "2.3.1 Number" "123" "123";
  check "2.3.1 Empty list" "()" "()";
  check "2.3.1 Singleton list" "(foo)" "(foo)";

  (* 2.3.2 Nested mixed list *)
  check "2.3.2 Nested mixed list" "(a b (c) (+ e) f)" "(a b (c) (+ e) f)";

  (* 2.3.3 Quote forms *)
  check "2.3.3 Quote form 1" "(quote a)" "(quote a)";
  check "2.3.3 Quote form 2" "'a" "(quote a)";
  check "2.3.3 Quote form 3" "'(a b (c d) 123)" "(quote (a b (c d) 123))";

  (* 2.3.4 Comments inside and after a list *)
  check "2.3.4 Comments inside and after a list" 
    "(a ; comment about a\n b (c) ;; comment about nested list\n d) ;; comment after list" 
    "(a b (c) d)";

  (* 2.3.5 Ill-formed list *)
  check "2.3.5 Ill-formed list" "(a b (c)" "Parse error";

  (* Final report*)
  Printf.printf "\n────────────────────────────\n";
  Printf.printf "  %d passed,  %d failed\n" !passes !failures;
  Printf.printf "────────────────────────────\n";
  if !failures > 0 then exit 1
