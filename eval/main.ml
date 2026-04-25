
type lam =
  | V   of string
  | Lam of string * lam
  | App of lam * lam

(* Combinatory Logic: variable, S/K/I combinators, application *)
type comb =
  | Vc   of string
  | S
  | K
  | I
  | Appc of comb * comb

(* Bracket abstraction: eliminates variable x from combinator term c *)
let rec abs_comb (x : string) (c : comb) : comb =
  match c with
  | Vc y when y = x -> I
  | Vc y            -> Appc (K, Vc y)
  | I               -> Appc (K, I)
  | K               -> Appc (K, K)
  | S               -> Appc (K, S)
  | Appc (c1, c2)   -> Appc (Appc (S, abs_comb x c1), abs_comb x c2)

(* Translate lambda term to combinatory logic term *)
let rec translate (m : lam) : comb =
  match m with
  | V x           -> Vc x
  | App (m1, m2)  -> Appc (translate m1, translate m2)
  | Lam (x, body) -> abs_comb x (translate body)

(* unstack: rebuilds term from stack, evaluating each element with wnf *)
and unstack (c : comb) (s : comb list) : comb =
  match s with
  | []       -> c
  | c2 :: rest -> unstack (Appc (c, wnf c2 [])) rest

(* wnf: weak normal form evaluator via stack machine.
   Applies one-step reduction rules; falls through to unstack when stuck. *)
and wnf (c : comb) (s : comb list) : comb =
  match c, s with
  | I, c1 :: s'              -> wnf c1 s'
  | K, c1 :: _ :: s'        -> wnf c1 s'
  | S, c1 :: c2 :: c3 :: s' -> wnf (Appc (Appc (c1, c3), Appc (c2, c3))) s'
  | Appc (c1, c2), s'       -> wnf c1 (c2 :: s')
  | _                        -> unstack c s
