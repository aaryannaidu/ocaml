open Types

type env = (string * typ) list
(* empty_env: An empty environment (an empty list of bindings). *)
let empty_env : env = [] 

(* add_binding: Adds a new variable binding to the environment. *)
let add_binding (id : string) (t : typ) (e : env) : env =
  (id, t) :: e

(* lookup_binding: Looks up the type of a variable in the environment. 
   Returns Some type if found, None otherwise. *)
let rec lookup_binding (id : string) (e : env) : typ option =
  match e with
  | [] -> None
  | (var, t) :: rest -> 
      if var = id then Some t 
      else lookup_binding id rest
