open JsonBadgerParse;;
open Pg_query;;
open JsonSqlParse;;
open SqlAnalyse;;
module H = BatHashtbl;;
module L = BatList;;
module O = BatOption;;
module A = BatArray;;
module S = BatString;;





let parseAllQueries filepath =
        let queriesInfo = parse_json_to_query_info filepath in
        let _ = Printf.eprintf "Parse queryInfo ok %d éléments parsés\n%!" (List.length queriesInfo) in
        (*Certaines requêtes font segfault la lib postgresql !*)
        let queriesInfoQuerystr = List.filter_map (fun q -> let res = Some(q.query) in O.bind res (fun e -> Some(q,e)))  queriesInfo in 
        let _ = Printf.eprintf "Recup queries OK \n%!" in
        let getAst contenu = Printf.eprintf "On parse : \n%s\n%!" contenu; (Pg_query.raw_parse contenu).parse_tree in
        let _ = Gc.full_major in
        let _ = Printf.eprintf "Parse query OK \n%!" in
        let queriesInfoQuerystrJsons = List.map (fun (qi,qs) -> qi,qs,getAst qs) queriesInfoQuerystr in
        let _ = Printf.eprintf "Recup AST JSON OK \n%!" in
        let _ = Gc.full_major in
        let queriesInfoQuerystrJsons = List.map (fun (qi,querystr,json) -> 
                        try qi,querystr,S.nreplace ~str:json  ~sub:"\\" ~by:""  |> Tiny_json.Json.parse 
                        with e -> Printf.eprintf "\n%s\n" json; qi,querystr,Null) queriesInfoQuerystrJsons in
        let _ = Printf.eprintf "Traitement AST JSON OK \n%!" in
        let _ = Gc.full_major in
        let queriesInfoQuerystrJsonsAst = List.filter_map (fun (qi,qs,json) -> try Some(qi,qs,JsonSqlParse.json2Grammar json |> L.hd) with e -> validJsonOfJsont json |> Printf.eprintf "\n%s\n%!"; None ) queriesInfoQuerystrJsons in
        let _ = Gc.full_major in
        let _ = Printf.eprintf "Recup AST ML OK : %d éléments\n%!" (L.length queriesInfoQuerystrJsonsAst) in
        (*let from =  List.map (fun astsrc -> let ast = L.hd astsrc in SqlAnalyse.getFrom ast) asts in*)
        L.iter (fun (qi,qs,ast) -> try SqlAnalyse.query_infoToSql qi ast 
                                   with e -> Printf.eprintf "Erreur=%s Backtrace:%s\n" (Printexc.to_string e) (Printexc.get_backtrace ()) 
                                   ) queriesInfoQuerystrJsonsAst
;;


let () =
        ignore( parseAllQueries Sys.argv.(1));;

