OCAMLBUILD = ocamlbuild

SOURCES = boolean.mli boolean.ml bvec.mli bvec.ml magic.mli magic.ml lambda.mli lambda.ml test.ml main.ml
RESULT = main.byte
OCAMLLDFLAGS = -g
PACKS = oUnit

all: $(SOURCES)
	$(OCAMLBUILD) $(RESULT) -pkgs $(PACKS)

clean:
	$(OCAMLBUILD) -clean
