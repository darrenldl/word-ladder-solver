SRCFILES = src/*.ml

OPAMFILES = *.opam

OCPINDENT = ocp-indent \
	--inplace \
	$(SRCFILES)

.PHONY: all
all :
	dune build @all

.PHONY: format
format :
	$(OCPINDENT)

.PHONY : clean
clean:
	dune clean