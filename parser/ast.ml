type atom =
  | Num of string       
  | Sym of string       
  | Id  of string       

type exp =
  | A of atom
  | L of exp list

(* for printing  expressions *) 
let show_atom a = 
  match a with
  | Num s -> s     
  | Sym s -> s     
  | Id  s -> s     

let rec show_exp e = 
  match e with
  | A a  -> show_atom a
  | L es -> "(" ^ String.concat " " (List.map show_exp es) ^ ")"