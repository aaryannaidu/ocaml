(* Core lambda calculus terms *)
type term = 
  | Var of string           (* Variable: x *)
  | App of term * term      (* Application: (e1 e2) *)
  | Lam of string * term    (* Abstraction: \x.e *)

  (* primitive data types *)
  | Int  of int             (* Integer literal: 0, 1, 2, ... *)
  | Bool of bool            (* Boolean literal: true, false *)

  (* arithmetic operations *)
  | Add of term * term      (* e1 + e2 *)
  | Sub of term * term      (* e1 - e2 *)
  | Mul of term * term      (* e1 * e2 *)

  (* comparison operations — produce Bool *)
  | Eq  of term * term      (* e1 = e2  *)
  | Lt  of term * term      (* e1 < e2  *)

  (* boolean logic *)
  | And of term * term      (* e1 && e2 *)
  | Or  of term * term      (* e1 || e2 *)
  | Not of term             (* not e    *)

  (* if-then-else*)
  | If  of term * term * term  

(* Convert a term to a human-readable string for debugging/printing *)
let rec to_string = function
  | Var x              -> x
  | App (t1, t2)       -> "(" ^ to_string t1 ^ " " ^ to_string t2 ^ ")"
  | Lam (x, t)         -> "(\\" ^ x ^ "." ^ to_string t ^ ")"
  | Int  n             -> string_of_int n
  | Bool b             -> string_of_bool b
  | Add (t1, t2)       -> "(" ^ to_string t1 ^ " + " ^ to_string t2 ^ ")"
  | Sub (t1, t2)       -> "(" ^ to_string t1 ^ " - " ^ to_string t2 ^ ")"
  | Mul (t1, t2)       -> "(" ^ to_string t1 ^ " * " ^ to_string t2 ^ ")"
  | Eq  (t1, t2)       -> "(" ^ to_string t1 ^ " = " ^ to_string t2 ^ ")"
  | Lt  (t1, t2)       -> "(" ^ to_string t1 ^ " < " ^ to_string t2 ^ ")"
  | And (t1, t2)       -> "(" ^ to_string t1 ^ " && " ^ to_string t2 ^ ")"
  | Or  (t1, t2)       -> "(" ^ to_string t1 ^ " || " ^ to_string t2 ^ ")"
  | Not t              -> "(not " ^ to_string t ^ ")"
  | If  (c, t1, t2)   -> "(if " ^ to_string c ^ " then " ^ to_string t1
                          ^ " else " ^ to_string t2 ^ ")"
