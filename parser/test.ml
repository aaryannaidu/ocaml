
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
  try
    let got = show_all (parse input) in
    if got = expected then begin
      Printf.printf "[PASS] %s\n" label;
      incr passes
    end else begin
      Printf.printf "[FAIL] %s\n  expected: %s\n  got:      %s\n" label expected got;
      incr failures
    end
  with
  | Parsing.Parse_error ->
      Printf.printf "[FAIL] %s  -- Parse error near: \"%s\"\n" label (String.escaped input);
      incr failures
  | Failure msg ->
      Printf.printf "[FAIL] %s  -- Lexer error: %s\n" label msg;
      incr failures

let () =

  (* Empty / whitespace  *)
  check "empty input"      ""            "";
  check "whitespace only"  "   \n\t  "  "";

  (* Atoms  *)
  check "integer"           "42"    "42";
  check "identifier"        "foo"   "foo";
  check "identifier dot"    "foo."  "foo.";
  check "true"              "t"     "t";

  (* Operator atoms  *)
  check "plus"   "+"    "+";
  check "minus"  "-"    "-";
  check "times"  "*"    "*";
  check "div"    "div"  "div";
  check "mod"    "mod"  "mod";
  check "eq"     "="    "=";
  check "neq"    "=/="  "=/=";
  check "lt"     "<"    "<";
  check "gt"     ">"    ">";
  check "leq"    "<="   "<=";
  check "geq"    ">="   ">=";

  (* Keyword atoms  *)
  check "quote kw"   "quote"   "quote";
  check "eq kw"      "eq"      "eq";
  check "atom kw"    "atom"    "atom";
  check "cons kw"    "cons"    "cons";
  check "cond kw"    "cond"    "cond";
  check "lambda kw"  "lambda"  "lambda";
  check "label kw"   "label"   "label";
  check "defun kw"   "defun"   "defun";

  (* CADR family  *)
  check "car"    "car"    "car";
  check "cdr"    "cdr"    "cdr";
  check "cadr"   "cadr"   "cadr";
  check "caddr"  "caddr"  "caddr";
  check "caadr"  "caadr"  "caadr";
  check "cddr"   "cddr"   "cddr";

  (* Empty list ()  *)
  check "nil / empty list"  "()"  "()";

  (* Simple lists  *)
  check "singleton"          "(foo)"        "(foo)";
  check "two identifiers"    "(foo bar)"    "(foo bar)";
  check "number in list"     "(42)"         "(42)";
  check "operator in list"   "(+)"          "(+)";
  check "empty list element" "(() ())"      "(() ())";

  (* Nested lists  *)
  check "one level nest"    "(a (b c))"           "(a (b c))";
  check "two sublists"      "((a b) (c d))"       "((a b) (c d))";
  check "deep nest"         "(a (b (c)))"         "(a (b (c)))";
  check "assignment eg 1"   "(a b (c) (+ e) f)"   "(a b (c) (+ e) f)";

  (* Arithmetic expressions  *)
  check "addition"       "(+ 1 2)"       "(+ 1 2)";
  check "subtraction"    "(- 10 3)"      "(- 10 3)";
  check "multiply"       "(* 4 5)"       "(* 4 5)";
  check "division"       "(div 10 2)"    "(div 10 2)";
  check "modulo"         "(mod 7 3)"     "(mod 7 3)";
  check "comparison lt"  "(< x 10)"      "(< x 10)";
  check "comparison geq" "(>= a b)"      "(>= a b)";
  check "nested arith"   "(+ (* 2 3) 4)" "(+ (* 2 3) 4)";

  (* Function definitions  *)
  check "lambda"   "(lambda (x) x)"               "(lambda (x) x)";
  check "defun"    "(defun square (x) (* x x))"   "(defun square (x) (* x x))";
  check "label"    "(label f (lambda (x) x))"     "(label f (lambda (x) x))";

  (* Quote (') shorthand  *)
  check "quote atom"      "'foo"        "(quote foo)";
  check "quote number"    "'42"         "(quote 42)";
  check "quote list"      "'(a b)"      "(quote (a b))";
  check "quote kw form"   "(quote foo)" "(quote foo)";

  (* Comments  *)
  check "single semicolon"    "(a ; comment\n b)"    "(a b)";
  check "double semicolon"    "(a ;; note\n b)"      "(a b)";
  check "triple semicolon"    "(a ;;; note\n b)"     "(a b)";
  check "quadruple semicolon" "(a ;;;; note\n b)"    "(a b)";
  check "comment between atoms"
    "foo ; first\nbar ; second\n"
    "foo\nbar";
  check "assignment eg 2"
    "(a  ; first element\n   b (c) ;; third element\n    d)"
    "(a b (c) d)";

  (* Multiple top-level expressions  *)
  check "two top-level"    "foo bar"         "foo\nbar";
  check "three top-level"  "foo (a b) 42"    "foo\n(a b)\n42";
  check "list then atom"   "(+ 1 2) result"  "(+ 1 2)\nresult";

  (* Complex multi-expression programs  *)

  (* defun then a call to it *)
  check "defun then call"
    "(defun square (x) (* x x)) (square 5)"
    "(defun square (x) (* x x))\n(square 5)";

  (* two defuns then a nested call *)
  check "two defuns then nested call"
    "(defun double (x) (+ x x)) (defun quad (x) (double (double x))) (quad 3)"
    "(defun double (x) (+ x x))\n(defun quad (x) (double (double x)))\n(quad 3)";

  (* label (recursive function) then a call *)
  check "label recursive factorial"
    "(label fact (lambda (n) (cond ((= n 0) 1) (t (* n (fact (- n 1))))))) (fact 5)"
    "(label fact (lambda (n) (cond ((= n 0) 1) (t (* n (fact (- n 1)))))))\n(fact 5)";

  (* cond expression — if/else style *)
  check "cond expression"
    "(cond ((= x 0) zero) ((< x 0) neg) (t pos))"
    "(cond ((= x 0) zero) ((< x 0) neg) (t pos))";

  (* nested cond inside defun *)
  check "defun with cond"
    "(defun sign (x) (cond ((= x 0) zero) ((< x 0) neg) (t pos)))"
    "(defun sign (x) (cond ((= x 0) zero) ((< x 0) neg) (t pos)))";

  (* multiple quoted expressions *)
  check "multiple quotes"
    "'foo '(a b) '()"
    "(quote foo)\n(quote (a b))\n(quote ())";

  (* cons / car / cdr operations together *)
  check "cons car cdr program"
    "(cons 1 (cons 2 ())) (car (cons a b)) (cdr (cons a b))"
    "(cons 1 (cons 2 ()))\n(car (cons a b))\n(cdr (cons a b))";

  (* cadr family in real usage *)
  check "cadr access"
    "(cadr mylist) (caddr mylist) (caadr nested)"
    "(cadr mylist)\n(caddr mylist)\n(caadr nested)";

  (* mixed comments throughout a multi-expression program *)
  check "program with comments"
    "; define square\n(defun square (x) (* x x)) ;; done\n; now call it\n(square 7)"
    "(defun square (x) (* x x))\n(square 7)";

  (* comment inside each expression + between expressions *)
  check "interleaved comments"
    "(+ ;; adding\n 1 ;; first arg\n 2) ;; result\n(* 3 4) ;; multiply"
    "(+ 1 2)\n(* 3 4)";

  (* deeply nested mixed list with numbers identifiers and operators *)
  check "deep mixed program"
    "(defun f (x y) (+ (* x x) (* y y))) (f (+ 1 2) (- 5 3))"
    "(defun f (x y) (+ (* x x) (* y y)))\n(f (+ 1 2) (- 5 3))";

  (* lambda immediately applied *)
  check "lambda call"
    "((lambda (x) (* x x)) 4)"
    "((lambda (x) (* x x)) 4)";

  (* program that uses eq and atom built-ins *)
  check "eq and atom"
    "(eq (atom x) t) (atom (cons a b))"
    "(eq (atom x) t)\n(atom (cons a b))";

  (* many top-level atoms and lists interleaved *)
  check "mixed atoms and lists"
    "42 foo (+ 1 2) bar 99 (cons a ())"
    "42\nfoo\n(+ 1 2)\nbar\n99\n(cons a ())";

  (* Edge cases  *)
  check "big number"     "99999999999999999999"  "99999999999999999999";
  check "nested empty"   "(())"                  "(())";
  check "op at toplevel" "+"                     "+";
  check "list of nils"   "(() () ())"            "(() () ())";

  (* Final report*)
  Printf.printf "\n────────────────────────────\n";
  Printf.printf "  %d passed,  %d failed\n" !passes !failures;
  Printf.printf "────────────────────────────\n";
  if !failures > 0 then exit 1
