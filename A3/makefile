make:
	ocamlc -o sheet.ml
	ocamlyacc a3.mly
	ocamlc -o a3.mli
	ocamlc -o a3.ml
	ocamllex a2.mll
	ocamlc -o a2.ml
	ocaml test.ml

build:
	ocamlbuild a2.cmo a3.cmo sheet.cmo

clean:
	rm a.out a2.ml a3.mli a3.ml
	rm *.c*
