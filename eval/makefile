.PHONY: all run clean

# Default target compiles the test binary
all: test_bin

# Compile main.ml and test.ml into the test_bin executable
test_bin: main.ml test.ml
	ocamlopt main.ml test.ml -o test_bin

# Compile (if necessary) and run the tests
run: test_bin
	./test_bin

# Clean up generated files (compiled objects, interfaces, and binaries)
clean:
	rm -f *.cmi *.cmo *.cmx *.o test_bin
