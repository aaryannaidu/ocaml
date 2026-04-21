# Makefile for Lambda Calculi Interpreters

OCAMLC = /opt/homebrew/bin/ocamlc
SOURCES = ast.ml krivine.ml secd.ml tests.ml main.ml
OBJECTS = $(SOURCES:.ml=.cmo)
TARGET = main

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(OCAMLC) -o $@ $(OBJECTS)

%.cmo: %.ml
	$(OCAMLC) -c $<

clean:
	rm -f *.cmo *.cmi $(TARGET)

run: all
	./$(TARGET)
