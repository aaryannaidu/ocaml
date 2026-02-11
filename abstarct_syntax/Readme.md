# Assignment 2: Expressions, Substitutions, and Predicates

## Overview
This project implements a formal system for expressions and predicates in OCaml, including signature validation, well-formedness checking, substitutions, and weakest precondition calculations.

## Design Decisions

### 1. Module Structure
The project is organized into four logical modules:
- **`Signature`**: Handles symbol definitions and signature validation.
- **`Expression`**: Defines the structure of terms (variables and nodes) and tree properties.
- **`Substitution`**: Implements substitution application, composition, and editing.
- **`Predicates`**: Extends the system to first-order logic formulas.

### 2. Efficiency and Data Structures
- **Array Usage**: As specified, sub-expressions in a `Node` are stored in an `exp array`. This allows for efficient $O(1)$ access to children at specific positions.
- **Set of Variables**: The `vars` function returns a sorted, unique list (`List.sort_uniq`) to ensure the result behaves like a formal set.
- **Association Lists**: Substitutions are represented as `(variable * exp) list`. This provides a simple and idiomatic way to handle partial functions from variables to expressions.

### 3. Substitution & Composition
- **Composition**: The `compose` function implements $s_1 \circ s_2$ by applying $s_1$ to all expressions in $s_2$ and then merging with remaining mappings from $s_1$ that aren't overridden.
- **In-place Mutation**: The `subst_inplace` function performs mutation by directly modifying the `args` array within `Node` constructors.

### 4. Position and Editing
- **Paths**: Positions are represented as `int list` (a path of child indices from the root).
- **Safety**: The `edit` function returns an `exp option`. This ensures that if a user provides an invalid path (e.g., trying to descend into a variable or an out-of-bounds index), the system fails gracefully rather than throwing an exception.

## Assumptions

- **Immutable Variables**: While `subst_inplace` can mutate the children of a `Node`, it cannot mutate a top-level `V var` expression into a different expression because the `exp` type constructor itself is immutable. Mutation is only possible within the `array` fields of a `Node`.
- **Signature Disjointness**: It is assumed that the expression signature $\Sigma$ and predicate signature $\Pi$ are managed as distinct lists, though they use the same underlying `symbol` representation.
- **Well-Formedness**: Functions like `ht`, `size`, and `vars` assume the input expressions are well-formed according to some signature, though they will function on any valid `exp` tree.
