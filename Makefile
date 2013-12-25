test: segments.cmx util.cmx main.cmx
	ocamlopt -o monoid str.cmxa $^

%.cmx: %.ml
	ocamlopt -c $<
	ocamlc -c $<

.PHONY: clean

clean:
	rm -f *.o *.cmx *.cmi *.cmo
