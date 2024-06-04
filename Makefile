top:
	ocamlfind ocamlc  -thread -linkpkg -package "tiny_json,batteries" JsonSqlParse.ml
	utop -init top.ml
build:
	ocamlfind ocamlopt  -thread -linkpkg -package "tiny_json,batteries" JsonSqlParse.ml
	ocamlfind ocamlopt -o pg_auditor -linkpkg -thread -package "pg_query,tiny_json,batteries" JsonSqlParse.cmx pg_auditor.ml

clean:
	rm *.cm* 
	rm pg_auditor

