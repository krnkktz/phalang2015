# Makefile

all:
	@ocamlc lex.ml syn.ml eval.ml show.ml top.ml

clean:
	@rm -fv *.cmi *.cmo a.out

type:
	@ocamlc -i lex.ml syn.ml

