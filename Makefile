test: patch.cmx textformat.cmx test.cmx
	ocamlopt -o $@ str.cmxa $^

apatch: patch.cmx textformat.cmx cmdline.cmx
	ocamlopt -o $@ str.cmxa $^

fuzztest: patch.cmx fuzz.cmx
	ocamlopt -o $@ str.cmxa $^

%.cmx: %.ml
	ocamlopt -c $<
	ocamlc -c $<

.PHONY: clean all

all: test apatch fuzztest

clean:
	rm -f test apply *.o *.cmx *.cmi *.cmo
