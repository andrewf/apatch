test: patch.cmx textformat.cmx test.cmx
	ocamlopt -o $@ str.cmxa $^

apply: patch.cmx textformat.cmx cmdline.cmx
	ocamlopt -o $@ str.cmxa $^

fuzztest: patch.cmx fuzz.cmx
	ocamlopt -o $@ str.cmxa $^

%.cmx: %.ml
	ocamlopt -c $<
	ocamlc -c $<

.PHONY: clean all

all: test apply fuzztest

clean:
	rm -f test apply *.o *.cmx *.cmi *.cmo
