open Types
open Env
open Ast

exception TypeError of string

(* infer returns a list of possible types for an expression.
   An empty list means no valid type exists => type error.
   Multiple types in the list means overloading is possible. *)
let rec infer (e : exp) (env : env) : typ list =
  match e with

  (* Atoms *)
  | A (Num _) -> [TInt]

  (* 't is the boolean true constant *)
  | A (Sym "t") -> [TBool]

  (* Quoted symbols are atoms; we treat them as Bool (atom) *)
  | A (Sym _) -> [TBool]

  (* Variables: look up in env *)
  | A (Id id) ->
    (match lookup_binding id env with
     | Some t -> [t]
     | None   -> raise (TypeError ("Unbound variable: " ^ id)))

  (* The empty list () *)
  | L [] -> [TBool; TList 0]

  (* quote wraps any expression — return Bool since quoted atoms are treated as symbols *)
  | L [A (Sym "quote"); _] -> [TBool]

  (* atom: (atom e) => Bool *)
  | L [A (Sym "atom"); arg] ->
    let ts = infer arg env in
    if ts = [] then raise (TypeError ("atom: ill-typed argument in: " ^ show_exp e));
    [TBool]

  (* eq: (eq e1 e2) => Bool, both args must share a type *)
  | L [A (Sym "eq"); e1; e2] ->
    let ts1 = infer e1 env in
    let ts2 = infer e2 env in
    let compatible =
      List.exists (fun t1 -> List.exists (fun t2 -> match_types t1 t2) ts2) ts1
    in
    if compatible then [TBool]
    else raise (TypeError ("eq: argument types do not match in: " ^ show_exp e))

  (* car/cdr are parsed as CADR_COMBO -> A(Id "car") / A(Id "cdr") *)
  | L [A (Id "car"); arg] ->
    let ts = infer arg env in
    if List.mem TAny ts then [TAny]
    else
      let valid = List.exists (function TList n -> n > 0 | _ -> false) ts in
      if valid then [TAny]
      else raise (TypeError ("car: expected List(n) with n > 0 in: " ^ show_exp e))

  (* cdr: (cdr e) where e : List(n), n > 0 => List(n-1) *)
  | L [A (Id "cdr"); arg] ->
    let ts = infer arg env in
    if List.mem TAny ts then [TAny]
    else
      let results =
        List.filter_map (function TList n when n > 0 -> Some (TList (n - 1)) | _ -> None) ts
      in
      if results = [] then
        raise (TypeError ("cdr: expected List(n) with n > 0 in: " ^ show_exp e));
      results

  (* cons: (cons e1 e2) where e2 : List(n) => List(n+1) *)
  | L [A (Sym "cons"); e1; e2] ->
    let ts1 = infer e1 env in
    let ts2 = infer e2 env in
    if ts1 = [] then raise (TypeError ("cons: ill-typed first argument in: " ^ show_exp e));
    if List.mem TAny ts2 then [TAny]
    else
      let results =
        List.filter_map (function TList n -> Some (TList (n + 1)) | _ -> None) ts2
      in
      if results = [] then
        raise (TypeError ("cons: second argument must be List(n) in: " ^ show_exp e));
      results

  (* cond: (cond (b1 e1) (b2 e2) ...) 
     Each clause must be a List(2) with a Bool condition.
     Design decision: all branches must return the same type (strong typing). *)
  | L (A (Sym "cond") :: clauses) ->
    if clauses = [] then raise (TypeError "cond: requires at least one clause");
    let branch_types = List.map (fun clause ->
      match clause with
      | L [cond_exp; body_exp] ->
        let cond_ts = infer cond_exp env in
        if not (List.mem TBool cond_ts) then
          raise (TypeError ("cond: condition is not Bool in clause: " ^ show_exp clause));
        infer body_exp env
      | _ -> raise (TypeError ("cond: each clause must be a List(2): " ^ show_exp clause))
    ) clauses in
    (* Intersect all branch types: all branches must agree on a common type *)
    let common =
      List.fold_left
        (fun acc ts -> List.filter (fun t -> List.exists (match_types t) ts) acc)
        (List.hd branch_types)
        (List.tl branch_types)
    in
    if common = [] then
      raise (TypeError ("cond: branches have incompatible types in: " ^ show_exp e));
    common

  (* Arithmetic: (+ e1 e2 ...) and ( * e1 e2 ...) *)
  | L (A (Sym op) :: args) when List.mem op ["+"; "*"] ->
    if List.length args < 2 then
      raise (TypeError (op ^ ": requires at least 2 arguments"));
    List.iter (fun arg ->
      let ts = infer arg env in
      if not (List.exists (match_types TInt) ts) then
        raise (TypeError (op ^ ": argument must be Int in: " ^ show_exp arg))
    ) args;
    [TInt]

  (* Binary arithmetic: (- e1 e2), (div e1 e2), (mod e1 e2) *)
  | L [A (Sym op); e1; e2] when List.mem op ["-"; "div"; "mod"] ->
    let check arg =
      let ts = infer arg env in
      if not (List.exists (match_types TInt) ts) then
        raise (TypeError (op ^ ": argument must be Int in: " ^ show_exp arg))
    in
    check e1; check e2;
    [TInt]

  (* Comparison: (> e1 e2), (< e1 e2), (>= e1 e2), (<= e1 e2) *)
  | L [A (Sym op); e1; e2] when List.mem op [">"; "<"; ">="; "<="] ->
    let check arg =
      let ts = infer arg env in
      if not (List.exists (match_types TInt) ts) then
        raise (TypeError (op ^ ": argument must be Int in: " ^ show_exp arg))
    in
    check e1; check e2;
    [TBool]

  (* not: (not e) => Bool *)
  | L [A (Id "not"); arg] ->
    let ts = infer arg env in
    if not (List.exists (match_types TBool) ts) then
      raise (TypeError ("not: argument must be Bool in: " ^ show_exp e));
    [TBool]

  (* and / or: (and e1 e2), (or e1 e2) => Bool *)
  | L [A (Id op); e1; e2] when List.mem op ["and"; "or"] ->
    let check arg =
      let ts = infer arg env in
      if not (List.exists (match_types TBool) ts) then
        raise (TypeError (op ^ ": argument must be Bool in: " ^ show_exp arg))
    in
    check e1; check e2;
    [TBool]

  (* lambda: (lambda (p1 ... pn) body) => List(n) -> ty *)
  | L [A (Sym "lambda"); L params; body] ->
    let param_names = List.map (function
      | A (Id s) -> s
      | other -> raise (TypeError ("lambda: parameter must be an identifier: " ^ show_exp other))
    ) params in
    let n = List.length param_names in
    (* Parameters get type TAny; their actual type is constrained by the body *)
    let env' = List.fold_left (fun acc p -> add_binding p TAny acc) env param_names in
    let ret_types = infer body env' in
    List.map (fun ret -> TFunction (n, ret)) ret_types

  (* label: (label f (lambda ...)) 
     Allows recursive functions: bind f with a placeholder first *)
  | L [A (Sym "label"); A (Id fname); (L [A (Sym "lambda"); L params; _] as lam)] ->
    let n = List.length params in
    (* Bind f as a function with placeholder return type so the body can refer to f *)
    let env' = add_binding fname (TFunction (n, TAny)) env in
    infer lam env'

  (* defun: (defun f (p1 ... pn) body) *)
  | L [A (Sym "defun"); A (Id fname); L params; body] ->
    let param_names = List.map (function
      | A (Id s) -> s
      | other -> raise (TypeError ("defun: parameter must be an identifier: " ^ show_exp other))
    ) params in
    let n = List.length param_names in
    let env' = List.fold_left (fun acc p -> add_binding p TAny acc) env param_names in
    (* Bind f itself for recursion *)
    let env'' = add_binding fname (TFunction (n, TAny)) env' in
    let ret_types = infer body env'' in
    List.map (fun ret -> TFunction (n, ret)) ret_types

  (* Function application: (f e1 ... en) *)
  | L (A (Id fname) :: args) ->
    (match lookup_binding fname env with
     | Some (TFunction (n, ret)) ->
       if List.length args <> n then
         raise (TypeError (fname ^ ": expected " ^ string_of_int n ^
                           " arguments, got " ^ string_of_int (List.length args)));
       (* Check each argument is well-typed *)
       List.iter (fun arg ->
         let ts = infer arg env in
         if ts = [] then raise (TypeError (fname ^ ": ill-typed argument: " ^ show_exp arg))
       ) args;
       [ret]
     | Some TAny ->
       (* If it's a dynamic parameter acting as a function, type check args and return Any *)
       List.iter (fun arg ->
         let ts = infer arg env in
         if ts = [] then raise (TypeError (fname ^ ": ill-typed argument: " ^ show_exp arg))
       ) args;
       [TAny]
     | Some other ->
       raise (TypeError (fname ^ " is not a function, has type: " ^ string_of_typ other))
     | None ->
       raise (TypeError ("Unbound function: " ^ fname)))

  (* Application of a lambda directly: ((lambda ...) e1 ... en) *)
  | L (lam :: args) ->
    let fun_types = infer lam env in
    let results = List.filter_map (function
      | TFunction (n, ret) when n = List.length args ->
        let args_ok = List.for_all (fun arg -> infer arg env <> []) args in
        if args_ok then Some ret else None
      | _ -> None
    ) fun_types in
    if results = [] then
      raise (TypeError ("Application: argument count or type mismatch in: " ^ show_exp e));
    results
  
  | _ -> raise (TypeError ("Unknown or ill-formed expression: " ^ show_exp e))

(* Top-level entry: returns the inferred types or raises TypeError *)
let typecheck (e : exp) : typ list =
  infer e empty_env
