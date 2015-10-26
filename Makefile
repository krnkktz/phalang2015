# Makefile

all: run ulrun

run:
	@ocamlbuild run.native

ulrun:
	@ocamlbuild ulrun.native

web:
	ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.syntax \
		-syntax camlp4o -linkpkg -g -o web.byte \
		lex.ml syn.ml builtin.ml iden.ml eval.ml ulambda.ml inter.ml web.ml
	js_of_ocaml web.byte
	@mkdir -pv webdir/
	@mv -v web.js webdir/
	@cp -rv pg/ webdir/

clean:
	@rm -rfv _build
	@rm -fv *.cmi *.cmo *.byte

mrproper: clean
	@rm -vf *.native

