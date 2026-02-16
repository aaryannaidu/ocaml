
type myBool = T | F

type sign = Neg | NonNeg

type bigint = sign * int list
(* Representational Invariant:
   - Each int in the list is between 0 and 9
   - Digits are stored most significant first
   - No unnecessary leading zeros (except for zero itself: (NonNeg, [0]))
*)

(* exceptions *)
exception Division_by_zero
exception Invalid_bigint

(* remove leading zeros *)
let rec rem_lead_zeros bignum = 
match bignum with
| [] -> [0]
| 0::rest -> rem_lead_zeros rest
| _ -> bignum

(*compare two lists of digits true if l1 >= l2*)
let rec compare_list l1 l2 = 
  match l1, l2 with
  | [] , [] -> T
  | [] , _ -> F
  | _ , [] -> T
  | h1::t1, h2::t2 -> if h1 > h2 then T else if h1 < h2 then F else compare_list t1 t2


(*add two lists of digits absolutely*) 
let add_list l1 l2 = 
  let rec adder l1 l2 carry = 
    match l1, l2 with
    | [] , [] -> if carry = 0 then [] else [carry]
    | h1::t1, [] -> adder t1 [] carry
    | [], h2::t2 -> adder [] t2 carry
    | h1::t1, h2::t2 -> adder t1 t2 (h1 + h2 + carry)
  in List.rev (adder (List.rev l1) (List.rev l2) 0)

(*subtract two lists of digits assuming l1 >= l2*) 
let sub_list l1 l2 = 
  let rec subber l1 l2 borrow = 
    match l1, l2 with
    | [] , [] -> if borrow = 0 then [] else [borrow]
    | h1::t1, [] -> subber t1 [] borrow
    | [], h2::t2 -> subber [] t2 borrow
    | h1::t1, h2::t2 -> subber t1 t2 (h1 - h2 - borrow)
  in List.rev (subber (List.rev l1) (List.rev l2) 0)

(*add two bigints*)
let add_big (s1, l1) (s2, l2) = 
  match s1, s2 with
  | (NonNeg, _) , (NonNeg, _) -> (NonNeg, add_list l1 l2)
  | (Neg, _) , (Neg, _) -> (Neg, add_list l1 l2)
  | (NonNeg, _) , (Neg, _) -> if (compare_list l1 l2) = T then (NonNeg, sub_list l1 l2) else (Neg, sub_list l2 l1)
  | (Neg, _) , (NonNeg, _) -> if (compare_list l1 l2) = T then (NonNeg, sub_list l1 l2) else (Neg, sub_list l2 l1)
(*sub 2 bigints*)
let sub_big (s1, l1) (s2, l2) = 
  match s1, s2 with
  | (NonNeg, _) , (NonNeg, _) -> if (compare_list l1 l2) = T then (NonNeg, sub_list l1 l2) else (Neg, sub_list l2 l1)
  | (Neg, _) , (Neg, _) -> if (compare_list l1 l2) = T then (NonNeg, sub_list l1 l2) else (Neg, sub_list l2 l1)
  | (NonNeg, _) , (Neg, _) -> (NonNeg, add_list l1 l2)
  | (Neg, _) , (NonNeg, _) -> (Neg, add_list l1 l2)

let mul_big (s1, l1) (s2, l2) = 
  let rec mul_list l1 l2 = 
    match l1, l2 with
    | [] , _ -> []
    | _ , [] -> []
    | h1::t1, h2::t2 -> add_list (mul_list t1 l2) (mul_list l1 t2)
  if s1=s2 then (NonNeg, mul_list l1 l2) else (Neg, mul_list l1 l2)