
OCAMLOPT = ocamlopt
GENERATED = parser.ml parser.mli lexer.ml

all: lithp_parser lithp_test

# generate lexer.ml from lexer.mll
lexer.ml: lexer.mll
	ocamllex lexer.mll

# generate parser.ml + parser.mli from parser.mly
parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly

# build the main parser binary
lithp_parser: ast.ml parser.mli parser.ml lexer.ml main.ml
	$(OCAMLOPT) -o lithp_parser ast.ml parser.mli parser.ml lexer.ml main.ml

# build the test binary
lithp_test: ast.ml parser.mli parser.ml lexer.ml test.ml
	$(OCAMLOPT) -o lithp_test ast.ml parser.mli parser.ml lexer.ml test.ml

# Run the tests
test: lithp_test
	./lithp_test

clean:
	rm -f $(GENERATED) *.o *.cmx *.cmi lithp_parser lithp_test
