
module Signature = struct
  (* Symbol type: (name, arity) *)
  type symbol = string * int
  
  (* Signature is a list of symbols that satisfies the following constraints:  
     1. No repeated symbols 
     2. Arity of each symbol is >= 0
  *)
  let check_sig signature = 
    let rec check_rep sig_list = 
      match sig_list with
      | [] -> true
      | (name, _) :: rest -> 
          let names = List.map fst rest in (* get the names of the rest of the symbols *)
          if List.mem name names then false else check_rep rest
    in
    let rec check_arity sig_list = 
      match sig_list with
      | [] -> true
      | (_, arity) :: rest -> 
          if arity < 0 then false else check_arity rest
    in
    check_rep signature && check_arity signature
end;;


module Expression = struct
  open Signature
  
  (* Variable type *)
  type variable = string
  
  (* Expression type *)
  type exp = 
    | V of variable                    (* Variable *)
    | Node of symbol * (exp array)     (* Symbol with children *)
  
  let rec wfexp signature exp = 
    match exp with
    | V _ -> true
    | Node (sym, args) ->
        let (name, arity) = sym in
        (* Check if the symbol exists in the signature with the correct arity *)
        let sig_match = List.exists (fun (n, a) -> n = name && a = arity) signature in
        (* Check if the number of arguments matches the arity *)
        let arity_match = Array.length args = arity in
        (* Check if all arguments are well-formed (recursively) *)
        let args_wf = Array.fold_left (fun acc arg -> acc && wfexp signature arg) true args in
        sig_match && arity_match && args_wf
  
  (* Height*)
  let rec ht exp = 
    match exp with
    | V _ -> 0 
    | Node (_, args) ->
        if Array.length args = 0 then 
          0 
        else
          let child_heights = Array.map ht args in
          1 + Array.fold_left max 0 child_heights
  
  (* Size *)
  let rec size exp = 
    match exp with
    | V _ -> 1
    | Node (_, args) ->
        (* Get the size of all children and add 1 for current node *)
        let child_sizes = Array.map size args in
        1 + Array.fold_left (+) 0 child_sizes
  
  (* Get the set of variables *)
  let rec vars exp = 
    match exp with
    | V var -> [var]
    | Node (_, args) ->
        (* Get variables from all children *)
        let child_vars = Array.map vars args in
        let all_vars = Array.fold_left (fun acum v -> acum @ v) [] child_vars in
        List.sort_uniq compare all_vars
end;;


module Substitution = struct
  open Expression

  type substitution = (variable * exp) list

  (* subst s exp: Apply substitution s to expression exp *) 
  let rec subst s exp = 
    match exp with 
    | V var -> 
       (match List.assoc_opt var s with
        | None -> V var  (* Variable not in substitution, return as-is *)
        | Some e -> e)   (* Variable found, return replacement *)
    | Node (sym, args) -> 
        let new_args = Array.map (fun arg -> subst s arg) args in
        Node (sym, new_args)

  (* compose s1 s2 = s1 o s2 *)
  let compose s1 s2 = 
    let comp_s2 = List.map (fun (var, exp) -> (var, subst s1 exp)) s2 in
    let remaining_s1 = List.filter (fun (var, _) -> List.assoc_opt var s2 = None) s1 in
    comp_s2 @ remaining_s1

  type position = int list

  (* Edit the subexpression at a given position *)
  (* Returns Some exp if position is valid, None otherwise *) 
  let rec edit pos new_exp exp = 
    match pos with 
    | [] -> Some new_exp  (* Reached target position *)
    | h :: rest -> 
        match exp with 
        | V _ -> None  
        | Node (sym, args) -> 
            if h < 0 || h >= Array.length args then 
              None  (* Invalid index *)
            else 
              (* Recursively edit the h-th child *)
              match edit rest new_exp args.(h) with
              | None -> None  (* Invalid position in subtree *)
              | Some new_child ->
                  let new_args = Array.copy args in
                  new_args.(h) <- new_child;  (* Update the h-th child *)
                  Some (Node (sym, new_args))

  (* In-place substitution that mutates the expression tree *)
  let rec subst_inplace s exp = 
    match exp with 
    | V _ -> 
        ()
    | Node (sym, args) -> 
        for i = 0 to Array.length args - 1 do
          match args.(i) with
          | V var ->
              (match List.assoc_opt var s with
               | None -> ()  (* Not in substitution, leave as-is *)
               | Some e -> args.(i) <- e)  (* Replace with substitution *)
          | Node _ as node ->
              subst_inplace s node
        done

end;; 
