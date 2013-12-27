test: patch.cmx textformat.cmx test.cmx
	ocamlopt -o test str.cmxa $^

apply: patch.cmx textformat.cmx cmdline.cmx
	ocamlopt -o apply str.cmxa $^

%.cmx: %.ml
	ocamlopt -c $<
	ocamlc -c $<

.PHONY: clean all

all: test apply

clean:
	rm -f test apply *.o *.cmx *.cmi *.cmo
