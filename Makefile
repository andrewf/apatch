test: patch.cmx main.cmx
	ocamlopt -o monoid str.cmxa $^

%.cmx: %.ml
	ocamlopt -c $<
	ocamlc -c $<

.PHONY: clean

clean:
	rm -f monoid *.o *.cmx *.cmi *.cmo
