let () =
  (* Step 1: Create a lexbuf from stdin.
     A lexbuf is just a cursor/buffer that the lexer reads characters from. *)
  let lexbuf = Lexing.from_channel stdin in

  (* Step 2: Run the parser.
     Parser.program is the entry point (our %start symbol).
     It takes two arguments:
       - Lexer.token  : the function to get the next token
       - lexbuf       : the source of characters
     It returns: Ast.exp list  (all top-level expressions in the input) *)
  try
    let exprs = Parser.program Lexer.token lexbuf in

    (* Step 3: Print the AST.
       For each top-level expression, show it on its own line. *)
    List.iter (fun e -> print_endline (Ast.show_exp e)) exprs

  with
  (* Parser couldn't match the grammar rules *)
  | Parsing.Parse_error ->
      Printf.eprintf "Parse error at token: %s\n"
        (Lexing.lexeme lexbuf)

  (* Lexer hit an unexpected character *)
  | Failure msg ->
      Printf.eprintf "Lexer error: %s\n" msg
