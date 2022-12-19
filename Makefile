SRCFILES = src/*.ml

OPAMFILES = *.opam

OCPINDENT = ocp-indent \
	--inplace \
	$(SRCFILES)

.PHONY: all
all :
	dune build @all

.PHONY: run
run:
	dune exec -- src/wlsolve.exe test want

.PHONY: format
format :
	$(OCPINDENT)

.PHONY : clean
clean:
	dune clean
