%{
  open Ast
%}

/* Token declarations  */
%token <string> INT            
%token <string> IDENT          
%token <string> CADR_COMBO    
%token LPAR RPAR               
%token PLUS MINUS TIMES        
%token DIV MOD                 
%token EQ NEQ LEQ GEQ LT GT   
%token QUOTE QUOTE_KW          
%token EQ_KW                   
%token ATOM CONS COND          
%token LAMBDA LABEL DEFUN      
%token TRUE NIL                
%token EOF

/* start symbol */
%start program

/* types for each non-terminal */
%type <Ast.exp list> program   
%type <Ast.exp list> exp_list  
%type <Ast.exp>      exp       
%type <Ast.atom>     atom     

%%
/* Grammar rules */

/* A program is expressions followed by EOF. */
program:
    | exp_list EOF { $1 }

/* A list of expressions (right-recursive) */
exp_list:
    | { [] }
    | exp exp_list { $1 :: $2 }

/* One expression */
exp:
    | atom          { A $1 }
    | LPAR exp_list RPAR { L $2 }
    | NIL                { L [] }       /* () is the empty list */
    | QUOTE exp          { L [A (Sym "quote"); $2] }  /* 'x => (quote x) */

atom:
    | INT           { Num $1 }
    | IDENT         { Id $1 }
    | CADR_COMBO    { Id $1 }
    | TRUE          { Sym "t" }
    | PLUS          { Sym "+" }
    | MINUS         { Sym "-" }
    | TIMES         { Sym "*" }
    | DIV           { Sym "div" }
    | MOD           { Sym "mod" }
    | EQ            { Sym "=" }
    | NEQ           { Sym "=/=" }
    | LT            { Sym "<" }
    | GT            { Sym ">" }
    | LEQ           { Sym "<=" }
    | GEQ           { Sym ">=" }
    | QUOTE_KW      { Sym "quote" }
    | EQ_KW         { Sym "eq" }
    | ATOM          { Sym "atom" }
    | CONS          { Sym "cons" }
    | COND          { Sym "cond" }
    | LAMBDA        { Sym "lambda" }
    | LABEL         { Sym "label" }
    | DEFUN         { Sym "defun" }