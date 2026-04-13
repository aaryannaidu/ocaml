{ open Parser }


rule token = parse
(* whitespace *)
    | [' ' '\t' '\n' '\r'] { token lexbuf }
(* comments *)
    | ';'+ [^ '\n']* '\n'?  { token lexbuf }
(* empty list - must come BEFORE '(' rule *)
    | "()" { NIL }
(* parentheses *)
    | "(" { LPAR }
    | ")" { RPAR }
    | "'" { QUOTE }
(* integers *)
    | ['0'-'9']+   { INT (Lexing.lexeme lexbuf) }
(* Math Operations *)
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { TIMES }
    | "div" { DIV }
    | "mod" { MOD }
    | "=/="  { NEQ }
    | "<="   { LEQ }
    | ">="   { GEQ }
    | '='    { EQ }
    | '<'    { LT }
    | '>'    { GT }
(* Keywords *)
    | "quote" { QUOTE_KW }
    | "eq" { EQ_KW }
    | "atom" { ATOM }
    | "cons" { CONS }
    | "cond" { COND }
    | 'c' ['a' 'd']+ 'r'   { CADR_COMBO (Lexing.lexeme lexbuf) }
(* Functions Keywords *)
    | "lambda" { LAMBDA }
    | "label" { LABEL }
    | "defun" { DEFUN }
(* indentifiers *)
    | "t"                              { TRUE }
    | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* '.'?   { IDENT (Lexing.lexeme lexbuf) }
    | eof  { EOF }
(* catch-all: anything not matched above is a lexical error *)
    | _    { failwith ("Unexpected token: " ^ Lexing.lexeme lexbuf) }
    
   
   