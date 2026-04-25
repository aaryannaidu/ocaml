# Lambda to Combinatory Logic Evaluator

This project implements a compiler that translates Lambda Calculus expressions into Combinatory Logic (CL), alongside a stack-based virtual machine to evaluate the resulting CL terms.

## Files

- `main.ml`: Contains the core data types and evaluation logic.
- `test.ml`: Contains an extensive suite of assertions to verify the implementation.
- `makefile`: Provides commands to build and run the tests.

## Running the Code

You can easily build and execute the tests using the included `makefile`:

```bash
# Compile the code and run the test suite
make run

# Clean up all compiled object files and binaries
make clean
```

## Implementation Details

### Data Types
- `lam`: Represents Lambda Calculus terms (Variables, Abstractions, Applications).
- `comb`: Represents Combinatory Logic terms (Variables, Applications, and the `S`, `K`, `I` combinators).

### Compilation (Lambda -> CL)
1. **`abs_comb (x, c)`**: Implements **bracket abstraction**. It eliminates the variable `x` from the combinator term `c`, converting lambda abstractions into pure combinations of `S`, `K`, and `I`.
2. **`translate m`**: Recursively traverses a lambda term `m`. When it encounters a lambda abstraction `Lam(x, body)`, it first translates the `body` and then applies `abs_comb` to eliminate `x`.

### Evaluation (Stack Machine)
Evaluation is performed by a stack-based virtual machine operating on Combinatory Logic terms, implemented via two mutually recursive functions:
1. **`wnf (c, s)`**: Attempts to apply the standard one-step reduction rules for Combinatory Logic (`I`, `K`, `S`) directly onto the stack `s`. If the term is an application `Appc(c1, c2)`, it pushes `c2` onto the stack and evaluates `c1`. If no reduction rules apply (e.g., it is a free variable or stuck state), it delegates to `unstack`.
2. **`unstack (c, s)`**: Rebuilds the evaluated term from the stack. It pops arguments off the stack, evaluates them to normal form using `wnf`, and reapplies them to the base term `c`.

## Tests
The `test.ml` file includes robust test coverage:
- **Basic Combinators:** Tests the fundamental behavior of `I`, `K`, and `S`.
- **Lambda Translations:** Tests boolean encodings (`true`, `false`) and function composition.
- **Church Numerals & Arithmetic:** Tests translation and evaluation of numbers 0-2.
