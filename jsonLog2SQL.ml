open JsonBadgerParse;;
open Pg_query;;
open JsonSqlParse;;


let parseAllQueries filepath =
        let queries = parse_json_to_query_info filepath in
        let _ = Printf.printf "Parse queryInfo ok %d éléments parsés\n%!" (List.length queries) in
        (*Certaines requêtes font segfault la lib postgresql !*)
        let queriestr = List.map getOneParsableQuery queries (*|> List.filter (fun s ->  try let res = BatString.find s "ir_act_report_xml" < 0 in Printf.printf "%s\n%!" s; res with e -> true)*) in
        let _ = Printf.printf "Recup queries OK \n%!" in
        let getAst contenu = Printf.printf "On parse : \n%s\n%!" contenu; (Pg_query.raw_parse contenu).parse_tree in
        let _ = Printf.printf "Parse query OK \n%!" in
        let jsons = List.map getAst queriestr in
        let _ = Printf.printf "Recup AST OK \n%!" in
        let asts = List.map (fun ast -> try BatString.nreplace ~str:ast  ~sub:"\\" ~by:""  |> Tiny_json.Json.parse with e -> Printf.printf "\n%s\n" ast; Null) jsons in
        let _ = Printf.printf "Traitement AST OK \n%!" in
        List.map (fun ast -> try JsonSqlParse.json2Grammar ast with e -> validJsonOfJsont ast |> Printf.printf "\n%s\n"; [] ) asts
;;


let () =
        ignore( parseAllQueries Sys.argv.(1));;

