# OCaml - Functional Programming

This repository contains OCaml projects exploring functional programming concepts. OCaml is a powerful, multi-paradigm programming language that emphasizes functional programming with strong static typing, type inference, and immutability. It's particularly well-suited for symbolic computation, formal verification, and building robust software systems.

## Projects

### [Sudoku Solver](./suduko_solver)
A SAT-based Sudoku solver implementation that converts Sudoku puzzles to CNF (Conjunctive Normal Form) and uses SAT solving techniques to find solutions.

### [Automated Theorem Proving](./abstarct_syntax)
An implementation of first-order logic abstract syntax trees, including support for signatures, complex expressions, substitutions (composition and in-place), and predicate logic formulas.

### [BigNum Module](./big_num)
A library for arbitrary-precision integer arithmetic (`BIGNUM`). 

**Objectives:**
- Overcome the limitations of OCaml's native 64-bit integers by menggunakan list-based digit representation.
- Implement a complete suite of arithmetic operations (Addition, Subtraction, Multiplication, Division, and Remainder) and comparison operators.
- Maintain a strict representational invariant (no leading zeros, normalized zero) for consistent arithmetic results.

**Use Cases:**
- **Scientific Computing**: Handling values that exceed standard word sizes (e.g., Factorials of large numbers like `100!`).
- **Cryptographic Foundations**: Large-number arithmetic is essential for encryption algorithms like RSA.
- **Symbolic Logic**: Providing a robust numerical foundation for automated theorem provers and formal verification tools.
