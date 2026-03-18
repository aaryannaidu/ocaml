type token =
  | LPAR
  | RPAR
  | QUOTE
  | TRUE
  | NIL           (* empty list / false *)
  | INT of string (* raw numeral digits as string; convert to BigNum.bigint in the evaluator *)
  | PLUS | MINUS | TIMES | DIV | MOD
  | EQ | GT | LT | LEQ | GEQ | NEQ   (* =/= *)
  | QUOTE_KW | EQ_KW      (* keyword 'quote' , 'eq' *)
  | ATOM |  CAR | CDR | CONS | COND
  | CADR_COMBO of string  (* c[ad]+r *)
  | LAMBDA | LABEL | DEFUN
  | IDENT of string       (* identifiers, optionally ending in . *)
  | EOF
