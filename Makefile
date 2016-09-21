.DEFAULT_GOAL := all

######################################################################

SRCDIR := src
include build-support/OCamlSrcs.makefile

SRCDIR := script_src
include build-support/OCamlSrcs.makefile

######################################################################
.PHONY: all

all: src/_build/access_log.cma analyse

analyse: script_src/_build/native_bin/analyse
	cp $< $@

clean:
	rm -rf $(BUILDDIRS)
	rm -f analyse
