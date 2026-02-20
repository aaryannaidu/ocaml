# BigNum Module - OCaml

This package implements a `BigNum` module for arbitrarily large integer arithmetic using a list-based representation.

## Data Representation
- **Type Definition**: `type bigint = sign * int list` where `sign = Neg | NonNeg`.
- **Representational Invariant**: 
    - Digits are stored as an `int list` in **most significant digit first** order.
    - Each integer in the list is between `0` and `9`.
    - No unnecessary leading zeros are permitted (e.g., `10` is `[1; 0]`, not `[0; 1; 0]`). Zero is uniquely represented as `(NonNeg, [0])`.

## Design Choices

### 1. Arithmetic Strategy
- **Addition/Subtraction**: Performed using the standard schoolbook algorithm. The lists are reversed to process digits from **least significant to most significant** for easier carry/borrow propagation, then reversed back for the final result.
- **Multiplication**: Implemented via partial products (single-digit multiplication) followed by shifted additions.
- **Division/Remainder**: Implemented using a **Long Division** algorithm. It iteratively brings down digits from the dividend, maintains a running remainder, and determines the quotient digit by repeated subtraction.

### 2. Comparisons
Comparison operations (`eq_big`, `gt_big`, `lt_big`, etc.) first evaluate the signs and list lengths (magnitude) before performing a recursive head-to-tail lexicographical comparison. This ensures $O(N)$ efficiency for numbers of length $N$.

### 3. Handling Zero
To maintain the representational invariant, a `rem_lead_zeros` helper function is applied after every operation. It ensures that results like $100 - 99$ result in `[1]` rather than `[0; 1]`, and that negative zero is always normalized to `(NonNeg, [0])`.

## Implementation Details
- **Sign Logic**: Signs are handled at the wrapper level for all operations. For example, `add_big` decides whether to call `add_list` or `sub_list` based on the relative signs and magnitudes of the operands.
- **Exceptions**: `Division_by_zero` is raised for quotient or remainder operations where the divisor is zero. `Invalid_bigint` is used for internal consistency checks.

## Functions Provided
- **Arithmetic**: `add_big`, `sub_big`, `mul_big`, `qt_big`, `rem_big`.
- **Unary**: `neg_big`, `abs_big`.
- **Comparison**: `eq_big`, `gt_big`, `lt_big`, `ge_big`, `le_big` (returns `myBool`).
- **Conversion/I/O**: `int_to_bigint`, `bigint_to_string`.
