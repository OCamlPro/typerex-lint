OCPBUILD=ocp-build
OCAMLOPT=ocamlopt.opt
OCAMLC=ocamlc
SRC=src

all:
	@if [ ! -d "_obuild" ]; then $(OCPBUILD) init; fi
	$(OCPBUILD)

clean:
	$(OCPBUILD) clean

cleanall: 
	@rm -rf _obuild/

