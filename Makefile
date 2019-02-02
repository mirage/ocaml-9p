
.PHONY: build clean test

build:
	dune build

test:
	dune runtest

doc:
	dune build @doc

install:
	dune install

uninstall:
	dune uninstall

clean:
	rm -rf _build
