test: segments.cmx main.cmx
	ocamlopt -o monoid $^

%.cmx: %.ml
	ocamlopt -c $<
