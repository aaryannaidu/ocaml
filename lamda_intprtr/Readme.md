# Lambda Calculus Interpreters

This sub-project implements two distinct abstract machines for evaluating Lambda Calculus expressions:
1. **The Krivine Machine**: Call-by-Name (Lazy Evaluation)
2. **The SECD Machine**: Call-by-Value (Eager Evaluation)

## Project Structure

- **`ast.ml`**: Defines the fundamental abstract syntax tree for pure lambda terms (Variables, Applications, Abstractions) as well as extended primitive types (Integers, Booleans, Arithmetic Ops, Conditionals). Includes helper functions to stringify terms.
- **`krivine.ml`**: Implements the Call-by-Name Krivine machine state engine. Evaluates lambda expressions lazily and features an `unpack` function to reconstruct final closures back into readable AST terms.
- **`secd.ml`**: Implements the Call-by-Value SECD machine. Features a custom compiler that translates standard AST terms into flat lists of operation codes (`opcodes`) which are then systematically processed through a Stack, Environment, Control, and Dump routine.
- **`tests.ml`**: A highly rigorous test bench. Verifies implementation using standard operations, Church logic (`fst`, `snd`, `not`), Church Arithmetic computations, and specifically tests Lazy vs Eager recursion limits (bypassing infinite Omega divergence natively in Krivine).
- **`main.ml`**: A solitary execution trigger designed to cleanly separate test logic from execution logic.
- **`makefile`**: Automates sequential OCaml compilation and testing.

## Key Design Decisions

1. **Environment & Closure Locality**  
Because the Krivine machine holds raw `term` representations inside closures while the SECD machine holds compiled `opcode list` representations, environments could not be abstracted globally. Therefore, environments and closures are maintained entirely organically (and mutually recursively) inside their respective files rather than within an isolated `env.ml`.

2. **Dual-Environment Architectures**  
Following the requirements, *both* abstract machines successfully implement operations utilizing two distinct environment map data types inherently:
   - **List-Based (`env_l`)**: An association dictionary `(string * closure) list`. Good for unpacking.
   - **Function-Based (`env_f`)**: Evaluated via active variables `string -> closure`. Harder to unpack but theoretically faster scaling.

3. **Sub-Expression Primitive Reduction**  
To properly process nested mathematical structures with lazy Call-by-Name semantics (ex. `if (< 3 10) then (* 5 4) else 0`), the Krivine engine leverages fully mutually recursive `step`, `run`, and abstract `reduce_sub` loops so nested AST operands are fully finalized immediately.

4. **Resilient Unpacking Engine**  
`unpack_l` is explicitly constructed to evaluate abstract unbound closures sequentially. If `krivine` scans to an unresolved variable, instead of crashing, it catches using `opt` mappings and binds it transparently allowing complete textual Lambda expressions `(\t.(\f.f))` back into the terminal.

## How to execute

```bash
# Compile the modules and execute the test suite
make run

# Clean all generated objects and binaries
make clean
```
