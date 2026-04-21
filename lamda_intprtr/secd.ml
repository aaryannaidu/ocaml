open Ast

(* Opcodes *)
type opcode =
  | LOOKUP of string
  | MkCLOS of string * opcode list
  | APP
  | RET
  | OP_CONST of term
  | OP_ADD | OP_SUB | OP_MUL
  | OP_EQ | OP_LT
  | OP_AND | OP_OR | OP_NOT
  | OP_IF of opcode list * opcode list

(* Compile AST terms into opcodes *)
let rec compile = function
  | Var x -> [LOOKUP x]
  | App (e1, e2) -> (compile e1) @ (compile e2) @ [APP]
  | Lam (x, e) -> [MkCLOS (x, compile e @ [RET])]
  | Int n -> [OP_CONST (Int n)]
  | Bool b -> [OP_CONST (Bool b)]
  | Add (e1, e2) -> (compile e1) @ (compile e2) @ [OP_ADD]
  | Sub (e1, e2) -> (compile e1) @ (compile e2) @ [OP_SUB]
  | Mul (e1, e2) -> (compile e1) @ (compile e2) @ [OP_MUL]
  | Eq (e1, e2) -> (compile e1) @ (compile e2) @ [OP_EQ]
  | Lt (e1, e2) -> (compile e1) @ (compile e2) @ [OP_LT]
  | And (e1, e2) -> (compile e1) @ (compile e2) @ [OP_AND]
  | Or (e1, e2) -> (compile e1) @ (compile e2) @ [OP_OR]
  | Not e -> (compile e) @ [OP_NOT]
  | If (cond, t1, t2) -> (compile cond) @ [OP_IF (compile t1, compile t2)]

(* List-based machine *)
type value_l =
  | VClos_l of string * opcode list * env_l
  | VTerm_l of term
and env_l = (string * value_l) list

type dump_l = (value_l list * env_l * opcode list) list

let empty_l : env_l = []
let extend_l env x v = (x, v) :: env
let lookup_l env x = List.assoc x env

let rec eval_l s (e: env_l) c (d: dump_l) =
  match c, s, d with
  | [], [v], _ -> v
  | LOOKUP x :: c', _, _ -> eval_l (lookup_l e x :: s) e c' d
  | MkCLOS (x, c1) :: c', _, _ -> eval_l (VClos_l (x, c1, e) :: s) e c' d
  | APP :: c', v2 :: VClos_l (x, c1, e1) :: s', _ ->
      eval_l [] (extend_l e1 x v2) c1 ((s', e, c') :: d)
  | RET :: _, v :: _, (s', e', c') :: d' -> eval_l (v :: s') e' c' d'
  | OP_CONST t :: c', _, _ -> eval_l (VTerm_l t :: s) e c' d
  | OP_ADD :: c', VTerm_l (Int b) :: VTerm_l (Int a) :: s', _ -> eval_l (VTerm_l (Int (a + b)) :: s') e c' d
  | OP_SUB :: c', VTerm_l (Int b) :: VTerm_l (Int a) :: s', _ -> eval_l (VTerm_l (Int (a - b)) :: s') e c' d
  | OP_MUL :: c', VTerm_l (Int b) :: VTerm_l (Int a) :: s', _ -> eval_l (VTerm_l (Int (a * b)) :: s') e c' d
  | OP_EQ :: c', VTerm_l (Int b) :: VTerm_l (Int a) :: s', _ -> eval_l (VTerm_l (Bool (a = b)) :: s') e c' d
  | OP_LT :: c', VTerm_l (Int b) :: VTerm_l (Int a) :: s', _ -> eval_l (VTerm_l (Bool (a < b)) :: s') e c' d
  | OP_AND :: c', VTerm_l (Bool b) :: VTerm_l (Bool a) :: s', _ -> eval_l (VTerm_l (Bool (a && b)) :: s') e c' d
  | OP_OR :: c', VTerm_l (Bool b) :: VTerm_l (Bool a) :: s', _ -> eval_l (VTerm_l (Bool (a || b)) :: s') e c' d
  | OP_NOT :: c', VTerm_l (Bool a) :: s', _ -> eval_l (VTerm_l (Bool (not a)) :: s') e c' d
  | OP_IF (c1, c2) :: c', VTerm_l (Bool b) :: s', _ ->
      if b then eval_l s' e (c1 @ c') d else eval_l s' e (c2 @ c') d
  | _ -> failwith "SECD/list: stuck execution state"

(* Function-based machine *)
type value_f =
  | VClos_f of string * opcode list * env_f
  | VTerm_f of term
and env_f = string -> value_f

type dump_f = (value_f list * env_f * opcode list) list

let empty_f : env_f = fun x -> failwith ("Unbound: " ^ x)
let extend_f env x v = fun y -> if y = x then v else env y

let rec eval_f s (e: env_f) c (d: dump_f) =
  match c, s, d with
  | [], [v], _ -> v
  | LOOKUP x :: c', _, _ -> eval_f (e x :: s) e c' d
  | MkCLOS (x, c1) :: c', _, _ -> eval_f (VClos_f (x, c1, e) :: s) e c' d
  | APP :: c', v2 :: VClos_f (x, c1, e1) :: s', _ ->
      eval_f [] (extend_f e1 x v2) c1 ((s', e, c') :: d)
  | RET :: _, v :: _, (s', e', c') :: d' -> eval_f (v :: s') e' c' d'
  | OP_CONST t :: c', _, _ -> eval_f (VTerm_f t :: s) e c' d
  | OP_ADD :: c', VTerm_f (Int b) :: VTerm_f (Int a) :: s', _ -> eval_f (VTerm_f (Int (a + b)) :: s') e c' d
  | OP_SUB :: c', VTerm_f (Int b) :: VTerm_f (Int a) :: s', _ -> eval_f (VTerm_f (Int (a - b)) :: s') e c' d
  | OP_MUL :: c', VTerm_f (Int b) :: VTerm_f (Int a) :: s', _ -> eval_f (VTerm_f (Int (a * b)) :: s') e c' d
  | OP_EQ :: c', VTerm_f (Int b) :: VTerm_f (Int a) :: s', _ -> eval_f (VTerm_f (Bool (a = b)) :: s') e c' d
  | OP_LT :: c', VTerm_f (Int b) :: VTerm_f (Int a) :: s', _ -> eval_f (VTerm_f (Bool (a < b)) :: s') e c' d
  | OP_AND :: c', VTerm_f (Bool b) :: VTerm_f (Bool a) :: s', _ -> eval_f (VTerm_f (Bool (a && b)) :: s') e c' d
  | OP_OR :: c', VTerm_f (Bool b) :: VTerm_f (Bool a) :: s', _ -> eval_f (VTerm_f (Bool (a || b)) :: s') e c' d
  | OP_NOT :: c', VTerm_f (Bool a) :: s', _ -> eval_f (VTerm_f (Bool (not a)) :: s') e c' d
  | OP_IF (c1, c2) :: c', VTerm_f (Bool b) :: s', _ ->
      if b then eval_f s' e (c1 @ c') d else eval_f s' e (c2 @ c') d
  | _ -> failwith "SECD/func: stuck execution state"

(* Runners *)
let run_list t = eval_l [] empty_l (compile t) []
let run_func t = eval_f [] empty_f (compile t) []
