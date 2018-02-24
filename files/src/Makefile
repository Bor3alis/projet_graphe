all: skeleton.cma
skeleton.cmi: ../skeleton.mli
	@ocamlc -I /usr/lib/ocaml/ocamlgraph graph.cma ../skeleton.mli
skeleton.cma: skeleton.cmi skeleton.ml
	@ocamlc -a -I /usr/lib/ocaml/ocamlgraph graph.cma skeleton.ml -o skeleton.cma
	@rm a.out
clean:
	@rm -f *.cm*
	@rm -f a.out

