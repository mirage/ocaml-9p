all:
	ocaml pkg/pkg.ml build --tests true -q
	ocaml pkg/pkg.ml test

clean:
	rm -rf _build
