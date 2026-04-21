open Ast

(* Types *)

type closure_l = Clos_l of term * env_l
and  env_l     = (string * closure_l) list

type closure_f = Clos_f of term * env_f
and  env_f     = string -> closure_f

(* List-based env helpers *)

let empty_l : env_l = []
let extend_l env x cl = (x, cl) :: env
let lookup_l env x =
  match List.assoc_opt x env with
  | Some cl -> cl
  | None    -> failwith ("Krivine/list: unbound variable " ^ x)

(* Function-based env helpers *)

let empty_f : env_f = fun x -> failwith ("Krivine/func: unbound variable " ^ x)
let extend_f env x cl = fun y -> if y = x then cl else env y

(* Krivine machine — list environment *)

let rec step_l (Clos_l (e, gamma)) stack =
  match e with
  | App (e1, e2)  -> Some (Clos_l (e1, gamma), Clos_l (e2, gamma) :: stack)
  | Var x         -> Some (lookup_l gamma x, stack)
  | Lam (x, body) -> (match stack with
      | []       -> None
      | cl :: s' -> Some (Clos_l (body, extend_l gamma x cl), s'))
  | Int _ | Bool _ -> None
  | Add (a, b) -> (match reduce_sub_l gamma a, reduce_sub_l gamma b with
      | Int x,  Int y  -> Some (Clos_l (Int  (x + y),  gamma), stack) | _ -> None)
  | Sub (a, b) -> (match reduce_sub_l gamma a, reduce_sub_l gamma b with
      | Int x,  Int y  -> Some (Clos_l (Int  (x - y),  gamma), stack) | _ -> None)
  | Mul (a, b) -> (match reduce_sub_l gamma a, reduce_sub_l gamma b with
      | Int x,  Int y  -> Some (Clos_l (Int  (x * y),  gamma), stack) | _ -> None)
  | Eq  (a, b) -> (match reduce_sub_l gamma a, reduce_sub_l gamma b with
      | Int x,  Int y  -> Some (Clos_l (Bool (x = y),  gamma), stack) | _ -> None)
  | Lt  (a, b) -> (match reduce_sub_l gamma a, reduce_sub_l gamma b with
      | Int x,  Int y  -> Some (Clos_l (Bool (x < y),  gamma), stack) | _ -> None)
  | And (a, b) -> (match reduce_sub_l gamma a, reduce_sub_l gamma b with
      | Bool x, Bool y -> Some (Clos_l (Bool (x && y), gamma), stack) | _ -> None)
  | Or  (a, b) -> (match reduce_sub_l gamma a, reduce_sub_l gamma b with
      | Bool x, Bool y -> Some (Clos_l (Bool (x || y), gamma), stack) | _ -> None)
  | Not a      -> (match reduce_sub_l gamma a with
      | Bool x -> Some (Clos_l (Bool (not x), gamma), stack)          | _ -> None)
  | If (c, t1, t2) -> (match reduce_sub_l gamma c with
      | Bool true  -> Some (Clos_l (t1, gamma), stack)
      | Bool false -> Some (Clos_l (t2, gamma), stack)
      | _          -> None)

and reduce_sub_l gamma t =
  let (Clos_l (v, _), _) = run_l (Clos_l (t, gamma)) [] in v

and run_l cl stack =
  match step_l cl stack with
  | None          -> (cl, stack)
  | Some (cl', s) -> run_l cl' s

(* Krivine machine — function environment (same logic, different env representation) *)

let rec step_f (Clos_f (e, gamma)) stack =
  match e with
  | App (e1, e2)  -> Some (Clos_f (e1, gamma), Clos_f (e2, gamma) :: stack)
  | Var x         -> Some (gamma x, stack)
  | Lam (x, body) -> (match stack with
      | []       -> None
      | cl :: s' -> Some (Clos_f (body, extend_f gamma x cl), s'))
  | Int _ | Bool _ -> None
  | Add (a, b) -> (match reduce_sub_f gamma a, reduce_sub_f gamma b with
      | Int x,  Int y  -> Some (Clos_f (Int  (x + y),  gamma), stack) | _ -> None)
  | Sub (a, b) -> (match reduce_sub_f gamma a, reduce_sub_f gamma b with
      | Int x,  Int y  -> Some (Clos_f (Int  (x - y),  gamma), stack) | _ -> None)
  | Mul (a, b) -> (match reduce_sub_f gamma a, reduce_sub_f gamma b with
      | Int x,  Int y  -> Some (Clos_f (Int  (x * y),  gamma), stack) | _ -> None)
  | Eq  (a, b) -> (match reduce_sub_f gamma a, reduce_sub_f gamma b with
      | Int x,  Int y  -> Some (Clos_f (Bool (x = y),  gamma), stack) | _ -> None)
  | Lt  (a, b) -> (match reduce_sub_f gamma a, reduce_sub_f gamma b with
      | Int x,  Int y  -> Some (Clos_f (Bool (x < y),  gamma), stack) | _ -> None)
  | And (a, b) -> (match reduce_sub_f gamma a, reduce_sub_f gamma b with
      | Bool x, Bool y -> Some (Clos_f (Bool (x && y), gamma), stack) | _ -> None)
  | Or  (a, b) -> (match reduce_sub_f gamma a, reduce_sub_f gamma b with
      | Bool x, Bool y -> Some (Clos_f (Bool (x || y), gamma), stack) | _ -> None)
  | Not a      -> (match reduce_sub_f gamma a with
      | Bool x -> Some (Clos_f (Bool (not x), gamma), stack)          | _ -> None)
  | If (c, t1, t2) -> (match reduce_sub_f gamma c with
      | Bool true  -> Some (Clos_f (t1, gamma), stack)
      | Bool false -> Some (Clos_f (t2, gamma), stack)
      | _          -> None)

and reduce_sub_f gamma t =
  let (Clos_f (v, _), _) = run_f (Clos_f (t, gamma)) [] in v

and run_f cl stack =
  match step_f cl stack with
  | None          -> (cl, stack)
  | Some (cl', s) -> run_f cl' s

(* Unpack: reconstruct an Ast.term from the final list-env closure.
   Free variables (like the dummy placeholder for a binder) safely fall back to Var x. *)

let rec unpack_l (Clos_l (e, gamma)) : term =
  match e with
  | Var x -> (match List.assoc_opt x gamma with
      | Some cl -> unpack_l cl
      | None    -> Var x)   (* free / placeholder variable — keep as-is *)
  | Lam (x, body) ->
      Lam (x, unpack_l (Clos_l (body, extend_l gamma x (Clos_l (Var x, empty_l)))))
  | App (e1, e2)   -> App (unpack_l (Clos_l (e1, gamma)), unpack_l (Clos_l (e2, gamma)))
  | Int _  | Bool _ -> e
  | Add (a, b) -> Add (unpack_l (Clos_l (a, gamma)), unpack_l (Clos_l (b, gamma)))
  | Sub (a, b) -> Sub (unpack_l (Clos_l (a, gamma)), unpack_l (Clos_l (b, gamma)))
  | Mul (a, b) -> Mul (unpack_l (Clos_l (a, gamma)), unpack_l (Clos_l (b, gamma)))
  | Eq  (a, b) -> Eq  (unpack_l (Clos_l (a, gamma)), unpack_l (Clos_l (b, gamma)))
  | Lt  (a, b) -> Lt  (unpack_l (Clos_l (a, gamma)), unpack_l (Clos_l (b, gamma)))
  | And (a, b) -> And (unpack_l (Clos_l (a, gamma)), unpack_l (Clos_l (b, gamma)))
  | Or  (a, b) -> Or  (unpack_l (Clos_l (a, gamma)), unpack_l (Clos_l (b, gamma)))
  | Not a      -> Not (unpack_l (Clos_l (a, gamma)))
  | If (c, t1, t2) ->
      If (unpack_l (Clos_l (c, gamma)),
          unpack_l (Clos_l (t1, gamma)),
          unpack_l (Clos_l (t2, gamma)))

(* Top-level entry points *)

let eval_list (e : term) : term =
  let (final_cl, _) = run_l (Clos_l (e, empty_l)) [] in
  unpack_l final_cl

(* For function env, we extract the raw term from the final closure (no traversable env to unpack) *)
let eval_func (e : term) : term =
  let (Clos_f (result_e, _), _) = run_f (Clos_f (e, empty_f)) [] in
  result_e
