# Makefile

all:
	ocamlc lex.ml syn.ml eval.ml show.ml top.ml

clean:
	@rm -fv *.cmi *.cmo

mrproper: clean
	@rm -fv a.out

