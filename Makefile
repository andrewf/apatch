test: segments.cmx util.cmx main.cmx
	ocamlopt -o monoid $^

%.cmx: %.ml
	ocamlopt -c $<
	ocamlc -c $<
