(* ========================================
   PART 1: SYMBOLS AND SIGNATURES
   ======================================== *)

module Signature = struct
  (* Symbol type: (name, arity) *)
  type symbol = string * int
  
  (* Signature is a list of symbols that satisfies the following constraints:  
     1. No repeated symbols 
     2. Arity of each symbol is >= 0
  *)
  let check_sig sig = 
    let rec check_rep sig = 
      match sig with
      | [] -> true
      | (name, _) :: rest -> 
          let names = List.map fst rest in (* get the names of the rest of the symbols *)
          if List.mem name names then false else check_rep rest
    in
    let rec check_arity sig = 
      match sig with
      | [] -> true
      | (_, arity) :: rest -> 
          if arity < 0 then false else check_arity rest
    in
    check_rep sig && check_arity sig
end;;


(* ========================================
   PART 2: EXPRESSIONS
   ======================================== *)

module Expression = struct
  open Signature
  
  (* Variable type *)
  type variable = string
  
  (* Expression type *)
  type exp = 
    | V of variable                    (* Variable *)
    | Node of symbol * (exp array)     (* Symbol with children *)
  
  (* Check if an expression is well-formed according to the signature *)
  let rec wfexp sig exp = 
    match exp with
    | V _ -> true
    | Node (sym, args) ->
        let (name, arity) = sym in
        (* Check if the symbol exists in the signature with the correct arity *)
        let sig_match = List.exists (fun (n, a) -> n = name && a = arity) sig in
        (* Check if the number of arguments matches the arity *)
        let arity_match = Array.length args = arity in
        (* Check if all arguments are well-formed (recursively) *)
        let args_wf = Array.fold_left (fun acc arg -> acc && wfexp sig arg) true args in
        sig_match && arity_match && args_wf
  
  (* Height of an expression tree *)
  let rec ht exp = 
    match exp with
    | V _ -> 0  (* Variables have height 0 *)
    | Node (_, args) ->
        if Array.length args = 0 then 
          0  (* Nullary symbols have height 0 *)
        else
          (* Get max height of children and add 1 *)
          let child_heights = Array.map ht args in
          1 + Array.fold_left max 0 child_heights
  
  (* Size (number of nodes) in an expression tree *)
  let rec size exp = 
    match exp with
    | V _ -> 1
    | Node (_, args) ->
        (* Get the size of all children and add 1 for current node *)
        let child_sizes = Array.map size args in
        1 + Array.fold_left (+) 0 child_sizes
  
  (* Get the set of variables appearing in an expression *)
  let rec vars exp = 
    match exp with
    | V var -> [var]
    | Node (_, args) ->
        (* Get variables from all children *)
        let child_vars = Array.map vars args in
        let all_vars = Array.fold_left (fun acc v -> acc @ v) [] child_vars in
        (* Return unique sorted list *)
        List.sort_uniq compare all_vars
end;;
