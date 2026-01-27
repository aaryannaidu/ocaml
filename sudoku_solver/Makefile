# Sudoku Solver Makefile

# Compiler and flags
OCAMLC = ocamlc
OCAML_FLAGS = 
LIBS = str.cma

# Executables
ENCODER = sudoku2cnf
DECODER = sol2grid

# Intermediate files
CNF_FILE = formula.cnf
SAT_OUTPUT = sat_output.txt

# Input and output files
INPUT = input.txt
OUTPUT = output.txt

# Default target: compile both programs
all: $(ENCODER) $(DECODER)

# Compile encoder
$(ENCODER): sudoku2cnf.ml
	$(OCAMLC) $(OCAML_FLAGS) -o $(ENCODER) $(LIBS) sudoku2cnf.ml

# Compile decoder
$(DECODER): sol2grid.ml
	$(OCAMLC) $(OCAML_FLAGS) -o $(DECODER) $(LIBS) sol2grid.ml

# Run the full pipeline with uniqueness check
run: all
	@echo "=== Running Sudoku Solver ==="
	./$(ENCODER) $(INPUT) > $(CNF_FILE)
	z3 -dimacs $(CNF_FILE) > $(SAT_OUTPUT)
	@if grep -q "s UNSATISFIABLE" $(SAT_OUTPUT); then \
		echo "No solution"; \
		echo "No solution" > $(OUTPUT); \
	else \
		echo "Solution found. Checking uniqueness..."; \
		./$(DECODER) $(SAT_OUTPUT) > $(OUTPUT); \
		positive_lits=$$(grep "^v " $(SAT_OUTPUT) | sed 's/^v //' | tr ' ' '\n' | grep -E "^[0-9]+" | grep -v "^0$$" | tr '\n' ' '); \
		block_clause=""; \
		for lit in $$positive_lits; do \
			block_clause="$$block_clause-$$lit "; \
		done; \
		block_clause="$${block_clause}0"; \
		cp $(CNF_FILE) formula_with_block.cnf; \
		echo "$$block_clause" >> formula_with_block.cnf; \
		z3 -dimacs formula_with_block.cnf > sat_output2.txt; \
		if grep -q "s SATISFIABLE" sat_output2.txt; then \
			echo "Multiple solutions exist"; \
		else \
			echo "Unique solution"; \
		fi; \
		rm -f formula_with_block.cnf sat_output2.txt; \
	fi
	@echo "=== Done ==="

# Clean intermediate and output files
clean:
	rm -f $(ENCODER) $(DECODER) *.cmi *.cmo $(CNF_FILE) $(SAT_OUTPUT) $(OUTPUT)

# Clean everything including executables
distclean: clean
	rm -f $(ENCODER) $(DECODER)

.PHONY: all run clean distclean
