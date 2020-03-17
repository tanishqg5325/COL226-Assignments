make:
	ocamlc sheet.ml
	ocamlyacc a3.mly
	ocamlc a3.mli
	ocamlc a3.ml
	ocamllex a2.mll
	ocamlc a2.ml
	ocamlc -o assignment4 a2.cmo a3.cmo sheet.cmo str.cma test.ml

clean:
	rm a2.ml a3.mli a3.ml a.out assignment4
	rm *.cm*
