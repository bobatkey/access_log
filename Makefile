.DEFAULT_GOAL := all

.PHONY: all lib native

OCB := ocamlbuild -use-ocamlfind -I src -I script_src

all: lib native

lib:
	$(OCB) access_log.cma access_log.cmxa

native:
	$(OCB) analyse.native

clean:
	rm -rf _build
	rm -f analyse.byte
	rm -f analyse.native
