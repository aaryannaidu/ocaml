# ASSIGNMENT 1 — TESTCASES 

NOTE:
You MUST translate these testcases into your own representation.
The following canonical constructors are used:
- Node (symbol, children)
- V (variable)
- Pred, And, Or, Not, T, F

## PART 1: check_sig

1. check_sig []

2. check_sig [("f", 2); ("g", 1); ("a", 0)]

3. check_sig [("f", 2); ("g", -3)]

4. check_sig [("f", 2); ("b", 1); ("f", 1)]

5. check_sig [("f", 0); ("g", 0)]

6. check_sig [("f", 1000)]


## PART 2A: wfexp

Given Signature:
S = [("f", 2); ("g", 1); ("a", 0)]

1. wfexp (Node ("f", [| Node ("g", [| V "x" |]); Node ("a", [||]) |]))

2. wfexp (Node ("f", [| V "x" |]))

3. wfexp (Node ("f", [| V "x"; V "y"; V "z" |]))

4. wfexp (Node ("h", [| V "x" |]))

5. wfexp (Node ("f", [| Node ("a", [| V "x" |]); V "y" |]))


## PART 2B: ht, size, vars

1. ht (Node ("f", [| Node ("g", [| V "x" |]); Node ("a", [||]) |]))

2. size (Node ("f", [| Node ("g", [| V "x" |]); Node ("a", [||]) |]))

3. vars (Node ("f", [| Node ("g", [| V "y" |]); V "x" |]))


## PART 3: subst, compose, edit, inplace_subst

Given:
sigma = [("fn",3); ("f",2); ("g",1); ("h",2); ("a",0); ("b",0); ("c",0)]

x = V "x"
y = V "y"
z = V "z"

a = Node("a",[||])
b = Node("b",[||])
c = Node("c",[||])

### 1. Substitution

s1 = [("x", Node("h",[|b; y|])); ("y", Node("g",[|a|]))]

e1 = Node("fn",[|Node("g",[|x|]); Node("h",[|x; y|]); z|])

Compute:
subst e1 s1


### 2. Composition

s1_comp = [("x", Node("g",[|y|]))]

s2_comp = [("x", b); ("y", Node("h",[|a; b|])); ("z", a)]

e2 = Node("h",[|x; z|])

Compute:
subst e2 (compose s1_comp s2_comp)


### 3. Edit by position

e3 = Node("fn",[|Node("g",[|x|]); c; Node("h",[|b; a|])|])

Compute:
edit e3 [2;1] (Node("g",[|c|]))


### 4. In-place substitution

e4 = Node("f",[|x; Node("g",[|x|])|])

s4 = [("x", Node("g",[|b|]))]

Compute:
inplace_subst e4 s4


## PART 5: wff, psubst, wp

Given:
pi = [("P",2); ("Q",1); ("R",0)]

a = Node("a",[||])
b = Node("b",[||])

x = V "x"
y = V "y"

### 1. wff checks

wff pi T

wff pi F

wff pi (Pred("R",[|a|]))


### 2. Predicate substitution

s_p = [("x", Node("h",[|b; y|])); ("y", Node("g",[|a|]))]

p_p = Or(Pred("Q",[|x|]), Pred("P",[|x; y|]))

Compute:
psubst p_p s_p


### 3. Weakest precondition

p_wp = And(Not(Pred("P",[|x; y|])), Pred("Q",[|x|]))

Compute:
wp "x" (Node("g",[|b|])) p_wp
