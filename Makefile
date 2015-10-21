# Makefile

all:
	ocamlc lex.ml syn.ml eval.ml builtin.ml top.ml file.ml

clean:
	@rm -fv *.cmi *.cmo

mrproper: clean
	@rm -fv a.out

