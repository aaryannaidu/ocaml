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

(* ── run and print test ── *)
let run_test label input expected_tokens =
  Printf.printf "Test: %s\n" label;
  Printf.printf "Input:\n%s\n" input;
  try
    let tokens = tokenise input in
    let token_strs = List.map string_of_token tokens in
    Printf.printf "Output:\n%s\n" (String.concat " " token_strs);
    if tokens = expected_tokens then
      Printf.printf "Result: [PASS]\n\n"
    else begin
      Printf.printf "Result: [FAIL]\n";
      Printf.printf "Expected:\n%s\n\n" (String.concat " " (List.map string_of_token expected_tokens))
    end
  with Failure msg ->
    Printf.printf "Error: %s\n\n" msg

let () =
  Printf.printf "================================================================\n";
  Printf.printf "SECTION 2.2: LEXICAL ANALYSIS\n";
  Printf.printf "================================================================\n\n";

  run_test "2.2.1 Arithmetic operators, numerals, parentheses"
    "(+ 12 (* 3 4) (- 10 5) (div 20 3) (mod 20 3))"
    [LPAR; PLUS; INT "12"; LPAR; TIMES; INT "3"; INT "4"; RPAR; 
     LPAR; MINUS; INT "10"; INT "5"; RPAR; LPAR; DIV; INT "20"; INT "3"; RPAR; 
     LPAR; MOD; INT "20"; INT "3"; RPAR; RPAR; EOF];

  run_test "2.2.2 Comparison operators"
    "(= 1 1) (=/= 1 2) (<= 2 3) (>= 3 2) (> 4 1) (< 1 4)"
    [LPAR; EQ; INT "1"; INT "1"; RPAR; LPAR; NEQ; INT "1"; INT "2"; RPAR;
     LPAR; LEQ; INT "2"; INT "3"; RPAR; LPAR; GEQ; INT "3"; INT "2"; RPAR;
     LPAR; GT; INT "4"; INT "1"; RPAR; LPAR; LT; INT "1"; INT "4"; RPAR; EOF];

  run_test "2.2.3 Constants, quote keyword, quote symbol, empty list"
    "(quote a) 'a '(a b c) t ()"
    [LPAR; QUOTE_KW; IDENT "a"; RPAR; QUOTE; IDENT "a"; QUOTE;
     LPAR; IDENT "a"; IDENT "b"; IDENT "c"; RPAR; TRUE; NIL; EOF];

  run_test "2.2.4 Primitive operations and compound car/cdr abbreviations"
    "(car x) (cdr x) (cadr x) (caadr x) (cdar x) (cdadr x) (cons a b) (cond (t a))"
    [LPAR; CADR_COMBO "car"; IDENT "x"; RPAR; LPAR; CADR_COMBO "cdr"; IDENT "x"; RPAR;
     LPAR; CADR_COMBO "cadr"; IDENT "x"; RPAR; LPAR; CADR_COMBO "caadr"; IDENT "x"; RPAR;
     LPAR; CADR_COMBO "cdar"; IDENT "x"; RPAR; LPAR; CADR_COMBO "cdadr"; IDENT "x"; RPAR;
     LPAR; CONS; IDENT "a"; IDENT "b"; RPAR; LPAR; COND; LPAR; TRUE; IDENT "a"; RPAR; RPAR; EOF];

  run_test "2.2.5 Comments and identifiers ending in dot"
    ";;;; file header comment\n\
     (defun append. (x y)\n\
     \  (cond ; inline comment after cond\n\
     \    ((null. x) y) ;; indented comment\n\
     \    (t (cons (car x) (append. (cdr x) y))))) ; final inline comment"
    [LPAR; DEFUN; IDENT "append."; LPAR; IDENT "x"; IDENT "y"; RPAR;
     LPAR; COND; LPAR; LPAR; IDENT "null."; IDENT "x"; RPAR; IDENT "y"; RPAR;
     LPAR; TRUE; LPAR; CONS; LPAR; CADR_COMBO "car"; IDENT "x"; RPAR;
     LPAR; IDENT "append."; LPAR; CADR_COMBO "cdr"; IDENT "x"; RPAR; IDENT "y"; RPAR;
     RPAR; RPAR; RPAR; RPAR; EOF]



