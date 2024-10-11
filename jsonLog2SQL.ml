open Jsonm2json;;
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
        (*Certaines requêtes font segfault la lib postgresql !
         TODO : pourquoi on extrait la chaine alors qu'elle dans le queryinfo ? Au cas où on va la chercher dans le sample ?*)
        let queriesInfoQuerystr = L.filter_map (fun q -> let res = Some(S.nreplace ~str:q.query ~sub:"(...)" ~by:"(1)") 
                                                        in O.bind res (fun e -> Some(q,e)))
                                                  queriesInfo in 
        let _ = Printf.eprintf "Recup queries OK \n%!" in
        let getAst contenu = (*Printf.eprintf "On parse : \n%s\n%!" contenu;*) (Pg_query.raw_parse contenu).parse_tree in
        let _ = Gc.compact in
        let _ = Printf.eprintf "Parse query OK \n%!" in
        let queriesInfoQuerystrJsons = L.map (fun (qi,qs) -> 
                                                (*if Random.int 511 = 11 then Printf.eprintf "GC !\n%!";Gc.compact();*)
                                                qi,qs,getAst qs) queriesInfoQuerystr in
        let _ = Printf.eprintf "Recup AST JSON OK \n%!" in
        let _ = Gc.compact in
        let queriesInfoQuerystrJsons = L.mapi (fun i -> fun (qi,querystr,json) -> if i mod 1000 = 0 then Printf.eprintf ".%!";
                        if i mod 100000 = 0 then begin Gc.compact(); Printf.eprintf "GC ! %!"; end;
                        try qi,querystr,S.nreplace ~str:json  ~sub:"\\" ~by:""  |> Jsonm2json.string_2_tinyjson 
                        with e -> Printf.eprintf "\nItération %d\n%s\n" i json; qi,querystr,Null) queriesInfoQuerystrJsons in
        let _ = Printf.eprintf "Traitement AST JSON OK \n%!" in
        let _ = Gc.compact in
        let queriesInfoQuerystrJsonsAst = L.filteri_map (fun i -> fun (qi,qs,json) -> 
                if Random.int 38511 = 11 then begin Printf.eprintf "GC !\n%!"; Gc.compact(); end;
                try Some(qi,qs,JsonSqlParse.json2Grammar json |> L.hd) 
                with e -> validJsonOfJsont json |> Printf.eprintf "\nAnalyse Itération %d\n%s\n%s\n%!" i (Printexc.to_string e); None ) queriesInfoQuerystrJsons in
        let _ = Gc.compact in
        let _ = Printf.eprintf "Recup AST ML OK : %d éléments\n%!" (L.length queriesInfoQuerystrJsonsAst) in
        (*let from =  List.map (fun astsrc -> let ast = L.hd astsrc in SqlAnalyse.getFrom ast) asts in*)
        L.iter (fun (qi,qs,ast) -> try SqlAnalyse.query_infoToSql qi ast 
                                   with e -> Printf.eprintf "Erreur=%s Backtrace:%s\n" (Printexc.to_string e) (Printexc.get_backtrace ()) 
                                   ) queriesInfoQuerystrJsonsAst
;;


let () =
        ignore( parseAllQueries Sys.argv.(1));;

