top:
	ocamlfind ocamlc  -g -thread -linkpkg -package "tiny_json,batteries,jsonm" jsonm2json.ml
	ocamlfind ocamlc  -thread -linkpkg -package "tiny_json,batteries,jsonm" jsonm2json.cmo JsonSqlParse.ml
	ocamlfind ocamlc  -thread -linkpkg -package "jsonm,batteries,tiny_json" jsonm2json.cmo jsonBadgerParse.ml
	ocamlfind ocamlc  -linkpkg -thread -package "pg_query,jsonm,tiny_json,batteries" jsonm2json.cmo JsonSqlParse.cmo jsonBadgerParse.cmo sqlAnalyse.ml
	utop -init top.ml
build:
	ocamlfind ocamlopt  -g -thread -linkpkg -package "tiny_json,batteries,jsonm" jsonm2json.ml
	ocamlfind ocamlopt  -g -thread -linkpkg -package "tiny_json,batteries,jsonm" jsonm2json.cmx JsonSqlParse.ml
	ocamlfind ocamlopt  -g -thread -linkpkg -package "jsonm,batteries,tiny_json" jsonm2json.cmx jsonBadgerParse.ml
	ocamlfind ocamlopt -g -o pg_auditor -linkpkg -thread -package "pg_query,tiny_json,batteries,jsonm" jsonm2json.cmx JsonSqlParse.cmx pg_auditor.ml
	ocamlfind ocamlopt  -g -linkpkg -thread -package "pg_query,jsonm,tiny_json,batteries,jsonm" jsonm2json.cmx JsonSqlParse.cmx jsonBadgerParse.cmx sqlAnalyse.ml
	ocamlfind ocamlopt -g -o parselog -linkpkg -thread -package "pg_query,jsonm,tiny_json,batteries,jsonm" jsonm2json.cmx JsonSqlParse.cmx jsonBadgerParse.cmx sqlAnalyse.cmx jsonLog2SQL.ml


clean:
	rm *.cm* 
	rm pg_auditor

