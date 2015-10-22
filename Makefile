# Makefile

all:
	@ocamlbuild phalang.native
	@mv -v phalang.native a.out

clean:
	@rm -rfv _build
	@rm -vf a.out

