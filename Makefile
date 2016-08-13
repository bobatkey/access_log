.DEFAULT_GOAL := all

######################################################################

SRCDIR := src
include build-support/OCamlSrcs.makefile

######################################################################
.PHONY: all

all: src/_build/logparse.cma

clean:
	rm -rf $(BUILDDIRS)
