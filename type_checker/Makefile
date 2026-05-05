OCAMLOPT  = /opt/homebrew/bin/ocamlopt
OCAMLLEX  = /opt/homebrew/bin/ocamllex
OCAMLYACC = /opt/homebrew/bin/ocamlyacc
GENERATED = parser.ml parser.mli lexer.ml

all: typechecker

lexer.ml: lexer.mll
	$(OCAMLLEX) lexer.mll

parser.ml parser.mli: parser.mly
	$(OCAMLYACC) parser.mly

typechecker: types.ml ast.ml env.ml typechecker.ml parser.mli parser.ml lexer.ml main.ml
	$(OCAMLOPT) -o typechecker types.ml ast.ml env.ml typechecker.ml parser.mli parser.ml lexer.ml main.ml

typechecker_test: types.ml ast.ml env.ml typechecker.ml parser.mli parser.ml lexer.ml test.ml
	$(OCAMLOPT) -o typechecker_test types.ml ast.ml env.ml typechecker.ml parser.mli parser.ml lexer.ml test.ml

# Run the tests
test: typechecker_test
	./typechecker_test

clean:
	rm -f $(GENERATED) *.o *.cmx *.cmi typechecker typechecker_test
