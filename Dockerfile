FROM docker.io/ocaml/opam
USER root
RUN apt-get update
RUN apt-get install --yes pkg-config
RUN opam install dune containers fmt
RUN opam install timedesc oseq seq
RUN opam install utop ocp-indent
RUN opam install alcotest
RUN opam install qcheck qcheck-alcotest
RUN opam install angstrom
