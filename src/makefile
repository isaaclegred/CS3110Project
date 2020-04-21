MODULES=IO
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=unix,oUnit,str,owl,csv

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

