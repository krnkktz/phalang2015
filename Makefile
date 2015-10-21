# Makefile

all:
	ocamlc lex.ml syn.ml builtin.ml iden.ml eval.ml top.ml file.ml

clean:
	@rm -fv *.cmi *.cmo

mrproper: clean
	@rm -fv a.out

