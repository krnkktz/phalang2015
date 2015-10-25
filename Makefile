# Makefile

all: run ulrun

run:
	@ocamlbuild run.native
ulrun:
	@ocamlbuild ulrun.native

clean:
	@rm -rfv _build

mrproper: clean
	@rm -vf *.native

