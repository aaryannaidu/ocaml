module BigNum = struct
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

(* compare_list l1 l2 returns T if l1 >= l2 (magnitude), F if l1 < l2 *)
let rec compare_list l1 l2 =
  let len1 = List.length l1 in
  let len2 = List.length l2 in
  if len1 > len2 then T
  else if len1 < len2 then F
  else
    match l1, l2 with
    | [] , []         -> T   
    | h1::t1, h2::t2  -> if h1 > h2 then T else if h1 < h2 then F else compare_list t1 t2
    | _               -> T  


(*add two lists of digits absolutely*) 
let add_list l1 l2 = 
  let rec adder l1 l2 carry = 
    match l1, l2 with
    | [] , [] -> if carry = 0 then [] else [carry]
    | h1::t1, [] -> 
        let sum = h1 + carry in
        (sum mod 10) :: adder t1 [] (sum / 10)
    | [], h2::t2 -> 
        let sum = h2 + carry in
        (sum mod 10) :: adder [] t2 (sum / 10)
    | h1::t1, h2::t2 -> 
        let sum = h1 + h2 + carry in
        (sum mod 10) :: adder t1 t2 (sum / 10)
  in List.rev (adder (List.rev l1) (List.rev l2) 0)

(*subtract two lists of digits assuming l1 >= l2*) 
let sub_list l1 l2 = 
  let rec subber l1 l2 borrow = 
    match l1, l2 with
    | [] , [] -> []
    | h1::t1, [] -> 
        let diff = h1 - borrow in
        if diff < 0 then (diff + 10) :: subber t1 [] 1 else diff :: subber t1 [] 0
    | [], h2::t2 -> raise Invalid_bigint  (* should not happen if l1 >= l2 *)
    | h1::t1, h2::t2 -> 
        let diff = h1 - h2 - borrow in
        if diff < 0 then (diff + 10) :: subber t1 t2 1 else diff :: subber t1 t2 0
  in rem_lead_zeros (List.rev (subber (List.rev l1) (List.rev l2) 0))

(*add two bigints*)
let add_big (s1, l1) (s2, l2) = 
  match s1, s2 with
  | NonNeg , NonNeg -> (NonNeg, add_list l1 l2)
  | Neg , Neg -> (Neg, add_list l1 l2)
  | NonNeg , Neg -> if (compare_list l1 l2) = T then (NonNeg, sub_list l1 l2) else (Neg, sub_list l2 l1)
  | Neg , NonNeg -> if (compare_list l2 l1) = T then (NonNeg, sub_list l2 l1) else (Neg, sub_list l1 l2)

(*sub 2 bigints*)
let sub_big (s1, l1) (s2, l2) = 
  match s1, s2 with
  | NonNeg , NonNeg -> if (compare_list l1 l2) = T then (NonNeg, sub_list l1 l2) else (Neg, sub_list l2 l1)
  | Neg , Neg -> if (compare_list l2 l1) = T then (NonNeg, sub_list l2 l1) else (Neg, sub_list l1 l2)
  | NonNeg , Neg -> (NonNeg, add_list l1 l2)
  | Neg , NonNeg -> (Neg, add_list l1 l2)

(*multiply two bigints*)
let mul_big (s1, l1) (s2, l2) = 
  (* multiply a list by a single digit *)
  let rec mul_digit digit lst carry = 
    match lst with
    | [] -> if carry = 0 then [] else [carry]
    | h::t -> 
        let prod = h * digit + carry in
        (prod mod 10) :: mul_digit digit t (prod / 10)
  in
  (* multiply list by another list *)
  let rec mul_list l1 l2 shift = 
    match l1 with
    | [] -> [0]
    | h::t -> 
        let partial = List.rev (mul_digit h (List.rev l2) 0) in
        let shifted = partial @ (List.init shift (fun _ -> 0)) in
        add_list shifted (mul_list t l2 (shift + 1))
  in
  let result = rem_lead_zeros (mul_list (List.rev l1) l2 0) in
  if s1 = s2 then (NonNeg, result) else (Neg, result)

(*divide two lists of digits and return (quotient, remainder)*)
let div_list dvnd dvsr =
  (* find how many times dvsr fits in cur: returns (count, leftover) *)
  let rec pred_next_qdigit cur dvr q_digit =
    if compare_list cur dvr = F then (q_digit, cur)  
    else pred_next_qdigit (sub_list cur dvr) dvr (q_digit + 1)
  in
  (*long divison algorithm: *)
  let rec div_helper dvnd rem qt=
    match dvnd with
    | [] ->
        (rem_lead_zeros qt, rem)
    | h :: t ->
        (* shift remainder left by one decimal place and bring down next digit h *)
        let new_rem =
          if rem = [0] then [h]   (* avoid spurious leading zero *)
          else rem @ [h]
        in
        (* find how many times dvsr fits into new_rem *)
        let (q_digit, leftover) = pred_next_qdigit new_rem dvsr 0 in
        div_helper t leftover (qt @ [q_digit])
  in
  let (q, r) = div_helper dvnd [] [] in
  (rem_lead_zeros q, rem_lead_zeros r)

(* return quotient of two bigints *)
let qt_big (s1, l1) (s2, l2) =
  if l2 = [0] then raise Division_by_zero
  else
    match s1, s2 with
    | NonNeg , NonNeg -> let (q, _) = div_list l1 l2 in (NonNeg, q)
    | Neg    , Neg    -> let (q, _) = div_list l1 l2 in (NonNeg, q)
    | NonNeg , Neg    -> let (q, _) = div_list l1 l2 in (Neg, q)
    | Neg    , NonNeg -> let (q, _) = div_list l1 l2 in (Neg, q)

(* return remainder of two bigints — sign follows the dividend *)
let rem_big (s1, l1) (s2, l2) =
  if l2 = [0] then raise Division_by_zero
  else
    let (_, r) = div_list l1 l2 in
    (s1, r)  



(*negation of a bigint*)
let neg_big (s, l) =
  match l with
  | [0] -> (NonNeg, [0])   (* zero has no sign *)
  | _   -> (match s with NonNeg -> (Neg, l) | Neg -> (NonNeg, l))

(*absolute value of a bigint*)
let abs_big (_, l) = (NonNeg, l)

(*equlaity of 2 bigints*)
let eq_big (s1, l1) (s2, l2) = 
  if s1 = s2 && l1 = l2 then T else F

(* strict greater-than: b1 > b2 *)
let gt_big (s1, l1) (s2, l2) =
  match s1, s2 with
  | NonNeg, Neg    -> T  
  | Neg,    NonNeg -> F   
  | NonNeg, NonNeg ->
      if compare_list l1 l2 = T && l1 <> l2 then T else F
  | Neg,    Neg    ->
      if compare_list l2 l1 = T && l1 <> l2 then T else F

(* strict less-than: b1 < b2 *)
let lt_big (s1, l1) (s2, l2) =
  match s1, s2 with
  | Neg,    NonNeg -> T
  | NonNeg, Neg    -> F
  | NonNeg, NonNeg ->
      if compare_list l2 l1 = T && l1 <> l2 then T else F
  | Neg,    Neg    ->
      if compare_list l1 l2 = T && l1 <> l2 then T else F

(* greater than or equal to: b1 >= b2 *)
let ge_big (s1, l1) (s2, l2) =
  match s1, s2 with
  | NonNeg, Neg    -> T
  | Neg,    NonNeg -> F
  | NonNeg, NonNeg -> compare_list l1 l2
  | Neg,    Neg    -> compare_list l2 l1   

(* less than or equal to: b1 <= b2 *)
let le_big (s1, l1) (s2, l2) =
  match s1, s2 with
  | Neg,    NonNeg -> T
  | NonNeg, Neg    -> F
  | NonNeg, NonNeg -> compare_list l2 l1  
  | Neg,    Neg    -> compare_list l1 l2   

let int_to_list n = 
  if n = 0 then [0] else 
    let rec int_to_list_helper n acc = 
      if n = 0 then acc else int_to_list_helper (n / 10) ((n mod 10) :: acc)
    in int_to_list_helper n []
(* convert int to bigint *)
let int_to_bigint n = 
  if n >= 0 then (NonNeg, int_to_list n) else (Neg, int_to_list (-n))

(* convert a digit list to a string *)
let print_list l =
  print_string (String.concat "" (List.map string_of_int l))

(* convert bigint to a string  *)
let bigint_to_string (s, l) =
  let digits = String.concat "" (List.map string_of_int l) in
  match s with
  | NonNeg -> digits
  | Neg    -> "-" ^ digits

(* print bigint to stdout *)
let print_bigint (s, l) =
  match s with
  | NonNeg -> print_list l
  | Neg    -> print_string "-"; print_list l


end (* module BigNum *)
