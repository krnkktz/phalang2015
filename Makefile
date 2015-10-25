# Makefile

all:
	@ocamlbuild run.native
	@mv -v run.native a.out

clean:
	@rm -rfv _build

mrproper: clean
	@rm -vf a.out

