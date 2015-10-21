# Makefile

all:
	ocamlopt lex.ml syn.ml builtin.ml iden.ml eval.ml top.ml file.ml

clean:
	@rm -fv *.cmi *.cmo *.cmx *.o

mrproper: clean
	@rm -fv a.out

