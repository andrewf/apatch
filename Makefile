test: segments.cmx util.cmx main.cmx
	ocamlopt -o monoid $^

%.cmx: %.ml
	ocamlopt -c $<
	ocamlc -c $<

.PHONY: clean

clean:
	rm *.o *.cmx *.cmi *.cmo
