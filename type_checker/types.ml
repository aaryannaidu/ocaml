(* type_checker/types.ml *)

(** 
   LITHP Type System Representation

   Basic types (from assignment):
   - TInt: For bigint constants and results of arithmetic operations.
   - TBool: For 't, the empty list () (when used as false), and logical results.
   - TList(n): Represents a list of length n (n >= 0).
   
   Extended types (Improvements for type checking):
   - TAny: Used for the first argument of `cons` (which can be any type)
           or for parameters in functions (lambda/defun) before their 
           exact type constraints are fully resolved.
   - TFunction(n, ret_typ): Represents a function from List(n) -> ret_typ.
                            The assignment states: "function is of type 
                            List(n) -> ty".
*)

type typ =
  | TInt
  | TBool
  | TList of int               (* List of exactly n elements *)
  | TFunction of int * typ     (* List(n) -> return type *)
  | TAny                       (* Can match with any type *)

(* Helper function to compare if two types are compatible.*)
let rec match_types t1 t2 =
  match (t1, t2) with
  | (TAny, _) | (_, TAny) -> true
  | (TInt, TInt) -> true
  | (TBool, TBool) -> true
  | (TList n1, TList n2) -> n1 = n2
  | (TFunction (n1, ret1), TFunction (n2, ret2)) -> 
      n1 = n2 && match_types ret1 ret2
  | _ -> false

(* Helper to print types for debugging, error messages, and final output.*)
let rec string_of_typ = function
  | TInt -> "Int"
  | TBool -> "Bool"
  | TList n -> "List(" ^ string_of_int n ^ ")"
  | TFunction (param_count, ret_typ) -> 
      "List(" ^ string_of_int param_count ^ ") -> " ^ string_of_typ ret_typ
  | TAny -> "Any"