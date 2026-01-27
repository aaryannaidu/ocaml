# Sudoku Solver using SAT

This project implements a Sudoku solver by encoding the puzzle as a Boolean satisfiability (SAT) problem in Conjunctive Normal Form (CNF), solving it using the Z3 theorem prover, and decoding the solution back to a Sudoku grid.

## Overview

The solver consists of three main components:

1. **Encoder** (`sudoku2cnf.ml`): Converts a Sudoku puzzle to CNF
2. **SAT Solver** (Z3): Solves the CNF formula
3. **Decoder** (`sol2grid.ml`): Converts the SAT solution back to a Sudoku grid

## Approach

### Variable Encoding

We use a **direct encoding** where each variable represents a specific assignment:
- Variable `x_{i,j,v}` is true if cell `(i,j)` contains value `v`
- Encoding formula: `var = i × size² + j × size + v`
- Where `i, j ∈ [0, size-1]` (0-indexed) and `v ∈ [1, size]` (1-indexed)

For a 9×9 Sudoku, this creates 729 variables (9 × 9 × 9).

### Constraints

The encoder generates the following types of clauses:

1. **Cell Constraints**:
   - Each cell must have at least one value: `x_{i,j,1} ∨ x_{i,j,2} ∨ ... ∨ x_{i,j,9}`
   - Each cell has at most one value: `¬x_{i,j,v1} ∨ ¬x_{i,j,v2}` for all pairs `v1 ≠ v2`

2. **Row Constraints**:
   - Each value appears at least once in each row
   - No duplicate values in any row

3. **Column Constraints**:
   - Each value appears at least once in each column
   - No duplicate values in any column

4. **Block Constraints**:
   - Each value appears at least once in each 3×3 block (or 4×4 for 16×16)
   - No duplicate values in any block

5. **Given Values**:
   - Pre-filled cells are encoded as unit clauses: `x_{i,j,v}` where `v` is the given value

### Grid Format

**Input** (`input.txt`):
- One row per line
- `.` or `0` for empty cells
- `1-9` for 9×9 Sudoku
- `1-9, A-F` for 16×16 Sudoku

Example (9×9):
```
.6.3..8.4
537.9....
.........
.......32
.16....5.
.........
.3.....94
........1
4.5.....6
```

**Output** (`output.txt`):
- Completed grid with all cells filled
- Same format as input but no empty cells

Example:
```
162357894
537194268
...
```

## Usage

### Prerequisites

- OCaml compiler (`ocamlc`)
- Z3 theorem prover

**Installing Z3**:
- Ubuntu: `sudo apt-get install z3`
- macOS: `brew install z3`
- Windows: Download binary and add to PATH

### Compilation

```bash
make
```

This compiles both `sudoku2cnf` and `sol2grid` executables.

### Running the Solver

```bash
make run
```

This executes the full pipeline:
1. Reads `input.txt`
2. Generates `formula.cnf` (DIMACS format)
3. Runs Z3 to produce `sat_output.txt`
4. Generates `output.txt` with the solved grid

### Manual Execution

You can also run each step manually:

```bash
# Step 1: Encode
./sudoku2cnf input.txt > formula.cnf

# Step 2: Solve
z3 -dimacs formula.cnf > sat_output.txt

# Step 3: Decode
./sol2grid sat_output.txt > output.txt
```

### Validation

Use the provided validation script:

```bash
ocamlc -o check_sudoku check_sudoku.ml
./check_sudoku output.txt input.txt
```

### Cleaning

```bash
# Remove intermediate files
make clean

# Remove all generated files including executables
make distclean
```

## Edge Cases

### 1. Unsolvable Puzzles

If the input puzzle has no valid solution (UNSAT):
- Z3 outputs "unsat" in `sat_output.txt`
- `sol2grid` detects this and prints an error message to stderr
- The program exits with code 1
- No `output.txt` is generated

### 2. 16×16 Sudoku

The solver automatically detects grid size:
- **Encoder**: Detects size from the first row length
- **Decoder**: Estimates size from the maximum variable number in the solution
- Block size is computed as `√size` (3 for 9×9, 4 for 16×16)
- Values 10-15 are represented as A-F in both input and output

**Example 16×16 input**:
```
.6.3..8.4.......
537.9...........
................
```

**Example 16×16 output**:
```
162357894ABCDEF0
537194268....... 
```

### 3. Multiple Solutions

The solver can detect if a Sudoku puzzle has more than one valid solution using a **blocking clause** technique:

1. **Find first solution**: The solver finds a valid assignment of values.
2. **Add blocking clause**: We take all the "true" variables from the first solution and create a new constraint that says the solver cannot pick the exact same combination again.
3. **Solve again**: If Z3 can still find a solution with this new constraint, it means the puzzle has **Multiple solutions**. If it cannot (UNSAT), the solution is **Unique**.

This logic is automated in the `Makefile`. When you run `make run`, it will tell you on the terminal whether the solution found is unique or if others exist.

## Implementation Details

### Type System

The code uses idiomatic OCaml type definitions:
- `type lit = int` - A literal (positive or negative integer)
- `type clause = lit list` - A clause (disjunction of literals)
- `type cnf = clause list` - CNF formula (conjunction of clauses)

### Functional vs. Imperative

While OCaml encourages functional programming, we use mutable references (`ref`) for:
- Building lists of clauses efficiently
- Storing grid size and block size
- Accumulating the CNF formula

This is a pragmatic choice for performance and clarity in constraint generation.

### DIMACS Format

The CNF is output in standard DIMACS format:
```
p cnf <num_vars> <num_clauses>
<lit1> <lit2> ... 0
<lit1> <lit2> ... 0
...
```

Each clause ends with `0`, and literals are space-separated integers.

## Files

- `sudoku2cnf.ml` - Encoder implementation
- `sol2grid.ml` - Decoder implementation
- `Makefile` - Build and run automation
- `README.md` - This file
- `input.txt` - Input Sudoku puzzle (user-provided)
- `output.txt` - Solved Sudoku grid (generated)
- `formula.cnf` - Intermediate CNF file (generated)
- `sat_output.txt` - Z3 solver output (generated)
- `check_sudoku.ml` - Validation script (provided)


