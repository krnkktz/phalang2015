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
	@chmod -v a+r web.js
	@mkdir -pv webdir/
	@mv -v web.js webdir/
	@cp -rv pg/ webdir/
	@cp -v README README.temp
	sed -i 's/$$/<br>/' README.temp
	@cp -v webdir/index.html.template webdir/index.html
	sed -e '/README/ {' -e 'r README.temp' -e 'd' -e '}' -i webdir/index.html
	@rm -v README.temp

clean:
	@rm -rfv _build
	@rm -fv *.cmi *.cmo *.byte

mrproper: clean
	@rm -vf *.native
	@rm -vf webdir/index.html webdir/web.js

