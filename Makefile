all:
	ocaml pkg/pkg.ml build

test:
	ocaml pkg/pkg.ml build --tests true

clean:
	rm -rf _build
