all:
	ocaml pkg/pkg.ml build
ifeq ($(OS),Windows_NT)
	cp _build/src/main.native 9ptool.exe
endif

test:
	ocaml pkg/pkg.ml build --tests true
	ocaml pkg/pkg.ml test

clean:
	rm -rf _build
