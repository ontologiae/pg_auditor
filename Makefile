top:
	ocamlfind ocamlc  -thread -linkpkg -package "tiny_json,batteries" JsonSqlParse.ml
	ocamlfind ocamlc  -thread -linkpkg -package "jsonm,batteries" jsonBadgerParse.ml
	ocamlfind ocamlc  -linkpkg -thread -package "pg_query,jsonm,tiny_json,batteries" JsonSqlParse.cmo jsonBadgerParse.cmo sqlAnalyse.ml
	utop -init top.ml
build:
	ocamlfind ocamlopt  -g -thread -linkpkg -package "tiny_json,batteries" JsonSqlParse.ml
	ocamlfind ocamlopt  -g -thread -linkpkg -package "jsonm,batteries" jsonBadgerParse.ml
	ocamlfind ocamlopt -g -o pg_auditor -linkpkg -thread -package "pg_query,tiny_json,batteries" JsonSqlParse.cmx pg_auditor.ml
	ocamlfind ocamlopt  -g -linkpkg -thread -package "pg_query,jsonm,tiny_json,batteries" JsonSqlParse.cmx jsonBadgerParse.cmx sqlAnalyse.ml
	ocamlfind ocamlopt -g -o parselog -linkpkg -thread -package "pg_query,jsonm,tiny_json,batteries" JsonSqlParse.cmx jsonBadgerParse.cmx sqlAnalyse.cmx jsonLog2SQL.ml


clean:
	rm *.cm* 
	rm pg_auditor

