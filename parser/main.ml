let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let exprs = Parser.program Lexer.token lexbuf in
    List.iter (fun e -> print_endline (Ast.show_exp e)) exprs

  with
  (* Parser couldn't match the grammar rules *)
  | Parsing.Parse_error ->
      Printf.eprintf "Parse error at token: %s\n"
        (Lexing.lexeme lexbuf)

  (* Lexer hit an unexpected character *)
  | Failure msg ->
      Printf.eprintf "Lexer error: %s\n" msg
