open Main

(* ── pretty-print a token ── *)
let string_of_token = function
  | LPAR          -> "LPAR"
  | RPAR          -> "RPAR"
  | QUOTE         -> "QUOTE"
  | TRUE          -> "TRUE"
  | NIL           -> "NIL"
  | INT s         -> "INT(" ^ s ^ ")"
  | PLUS          -> "PLUS"
  | MINUS         -> "MINUS"
  | TIMES         -> "TIMES"
  | DIV           -> "DIV"
  | MOD           -> "MOD"
  | EQ            -> "EQ"
  | GT            -> "GT"
  | LT            -> "LT"
  | LEQ           -> "LEQ"
  | GEQ           -> "GEQ"
  | NEQ           -> "NEQ"
  | QUOTE_KW      -> "QUOTE_KW"
  | EQ_KW         -> "EQ_KW"
  | ATOM          -> "ATOM"
  | CAR           -> "CAR"
  | CDR           -> "CDR"
  | CONS          -> "CONS"
  | COND          -> "COND"
  | CADR_COMBO s  -> "CADR_COMBO(" ^ s ^ ")"
  | LAMBDA        -> "LAMBDA"
  | LABEL         -> "LABEL"
  | DEFUN         -> "DEFUN"
  | IDENT s       -> "IDENT(" ^ s ^ ")"
  | EOF           -> "EOF"

(* ── tokenise a string into a list of tokens ── *)
let tokenise input =
  let lexbuf = Lexing.from_string input in
  let rec loop acc =
    let tok = Lexer.token lexbuf in
    if tok = EOF then List.rev (EOF :: acc)
    else loop (tok :: acc)
  in
  loop []

(* ── test counters ── *)
let passed = ref 0
let failed = ref 0

(* ── assert that input tokenises to exactly expected ── *)
let assert_tokens label input expected =
  let got = tokenise input in
  if got = expected then begin
    Printf.printf "  [PASS] %s\n" label;
    incr passed
  end else begin
    Printf.printf "  [FAIL] %s\n" label;
    Printf.printf "         expected: %s\n"
      (String.concat " " (List.map string_of_token expected));
    Printf.printf "         got:      %s\n"
      (String.concat " " (List.map string_of_token got));
    incr failed
  end

(* ── assert that the first token of input is expected ── *)
let assert_first label input expected =
  let got = List.hd (tokenise input) in
  if got = expected then begin
    Printf.printf "  [PASS] %s\n" label;
    incr passed
  end else begin
    Printf.printf "  [FAIL] %s\n" label;
    Printf.printf "         expected first: %s\n" (string_of_token expected);
    Printf.printf "         got first:      %s\n" (string_of_token got);
    incr failed
  end

(* ══════════════════════════════════════════════════════════ *)

let () =

  (* ── Section 1: Structural characters ── *)
  Printf.printf "\n── Structural Characters ──\n";
  assert_tokens "left paren"  "("  [LPAR; EOF];
  assert_tokens "right paren" ")"  [RPAR; EOF];
  assert_tokens "quote char"  "'"  [QUOTE; EOF];

  (* ── Section 2: Constants ── *)
  Printf.printf "\n── Constants ──\n";
  assert_tokens "true"       "t"   [TRUE; EOF];
  assert_tokens "nil / ()"  "()"  [NIL; EOF];

  (* ── Section 3: BigInt numerals ── *)
  Printf.printf "\n── BigInt Numerals ──\n";
  assert_first "zero"                    "0"                            (INT "0");
  assert_first "small int"              "42"                           (INT "42");
  assert_first "large int"              "99999999999999999999"         (INT "99999999999999999999");
  assert_first "very large bigint"      "123456789012345678901234567890"
                                                                       (INT "123456789012345678901234567890");
  assert_tokens "bigint in expression"  "(+ 999999999999999 1)"
    [LPAR; PLUS; INT "999999999999999"; INT "1"; RPAR; EOF];
  (* Confirm stored as raw string — NOT converted to int (no overflow) *)
  assert_first "bigint stored as string, not int"
    "100000000000000000000000000000"
    (INT "100000000000000000000000000000");

  (* ── Section 4: Arithmetic operators ── *)
  Printf.printf "\n── Arithmetic Operators ──\n";
  assert_first "plus"   "+"   PLUS;
  assert_first "minus"  "-"   MINUS;
  assert_first "times"  "*"   TIMES;
  assert_first "div"    "div" DIV;
  assert_first "mod"    "mod" MOD;

  (* ── Section 5: Comparison operators ── *)
  Printf.printf "\n── Comparison Operators ──\n";
  assert_first "eq"   "="   EQ;
  assert_first "lt"   "<"   LT;
  assert_first "gt"   ">"   GT;
  assert_first "leq"  "<="  LEQ;
  assert_first "geq"  ">="  GEQ;
  assert_first "neq"  "=/=" NEQ;

  (* ── Section 6: Primitive keywords ── *)
  Printf.printf "\n── Primitive Keywords ──\n";
  assert_first "quote"  "quote"  QUOTE_KW;
  assert_first "atom"   "atom"   ATOM;
  assert_first "eq kw"  "eq"     EQ_KW;
  assert_first "cons"   "cons"   CONS;
  assert_first "cond"   "cond"   COND;

  (* ── Section 7: car/cdr compound forms ── *)
  Printf.printf "\n── car/cdr Compound Forms ──\n";
  assert_first "car"   "car"   (CADR_COMBO "car");
  assert_first "cdr"   "cdr"   (CADR_COMBO "cdr");
  assert_first "cadr"  "cadr"  (CADR_COMBO "cadr");
  assert_first "caadr" "caadr" (CADR_COMBO "caadr");
  assert_first "cdar"  "cdar"  (CADR_COMBO "cdar");
  assert_first "cdadr" "cdadr" (CADR_COMBO "cdadr");
  assert_first "caddr" "caddr" (CADR_COMBO "caddr");

  (* ── Section 8: Definition keywords ── *)
  Printf.printf "\n── Definition Keywords ──\n";
  assert_first "lambda" "lambda" LAMBDA;
  assert_first "label"  "label"  LABEL;
  assert_first "defun"  "defun"  DEFUN;

  (* ── Section 9: Identifiers (with optional dot) ── *)
  Printf.printf "\n── Identifiers ──\n";
  assert_first "plain ident"   "myFunc"   (IDENT "myFunc");
  assert_first "ident with ."  "list."    (IDENT "list.");
  assert_first "null."         "null."    (IDENT "null.");
  assert_first "eval."         "eval."    (IDENT "eval.");
  assert_first "append."       "append."  (IDENT "append.");
  assert_first "assoc."        "assoc."   (IDENT "assoc.");
  assert_first "evlis."        "evlis."   (IDENT "evlis.");

  (* ── Section 10: Comments ignored ── *)
  Printf.printf "\n── Comments (should be silently consumed) ──\n";
  assert_tokens "inline ;"    "(+ 1 2) ; ignored\n"  [LPAR; PLUS; INT "1"; INT "2"; RPAR; EOF];
  assert_tokens "double ;;"   ";; whole line\n42"     [INT "42"; EOF];
  assert_tokens "triple ;;;"  ";;; section\n42"       [INT "42"; EOF];
  assert_tokens "quad ;;;;"   ";; file header\n42"    [INT "42"; EOF];

  (* ── Section 11: Whitespace ignored ── *)
  Printf.printf "\n── Whitespace (should be silently consumed) ──\n";
  assert_tokens "spaces tabs newlines"
    "  (  \t+\n1\r\n2  )  "
    [LPAR; PLUS; INT "1"; INT "2"; RPAR; EOF];

  (* ── Section 12: Full LITHP programs ── *)
  Printf.printf "\n── Full LITHP Expressions ──\n";
  assert_tokens "defun with bigint args"
    "(defun add. (x y) (+ x y))"
    [LPAR; DEFUN; IDENT "add."; LPAR; IDENT "x"; IDENT "y"; RPAR;
     LPAR; PLUS; IDENT "x"; IDENT "y"; RPAR; RPAR; EOF];

  assert_tokens "cond expression"
    "(cond ((= x 0) t) (t ()))"
    [LPAR; COND; LPAR; LPAR; EQ; IDENT "x"; INT "0"; RPAR; TRUE; RPAR;
     LPAR; TRUE; NIL; RPAR; RPAR; EOF];

  assert_tokens "quote expression"
    "'(1 2 3)"
    [QUOTE; LPAR; INT "1"; INT "2"; INT "3"; RPAR; EOF];

  assert_tokens "bigint arithmetic in LITHP"
    "(+ 99999999999999999999 1)"
    [LPAR; PLUS; INT "99999999999999999999"; INT "1"; RPAR; EOF];

  (* ── Final summary ── *)
  Printf.printf "\n══════════════════════════════\n";
  Printf.printf "Results: %d passed, %d failed\n" !passed !failed;
  if !failed = 0 then
    Printf.printf "All tests PASSED ✓\n"
  else
    Printf.printf "Some tests FAILED ✗\n";
  Printf.printf "══════════════════════════════\n"
