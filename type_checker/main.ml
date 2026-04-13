open Ast
open Types
open Typechecker

let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    (* Parse the input stream into a list of expressions using your parser *)
    let exprs = Parser.program Lexer.token lexbuf in
    
    (* Check types for each parsed expression *)
    List.iter (fun e -> 
      print_endline ("Checking Expression: " ^ show_exp e);
      let types = typecheck e in
      
      (* Map inferred types to strings and print *)
      let types_str = String.concat ", " (List.map string_of_typ types) in
      print_endline ("Inferred Types: [" ^ types_str ^ "]\n")
    ) exprs

  with
  | Parsing.Parse_error ->
      Printf.eprintf "Parse error at token: %s\n" (Lexing.lexeme lexbuf)
  | Failure msg ->
      Printf.eprintf "Lexer error: %s\n" msg
  (* Catch our custom TypeError from typechecker.ml *)
  | TypeError msg ->
      Printf.eprintf "Type Error: %s\n" msg
