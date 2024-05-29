open Tiny_json.Json
open Pg_query


let sql2 = "
With g(n) as ( select generate_series(1,1000) )
SELECT to_char(e.edateevent, 'HH24hMI (dy DD/MM)'::text) AS edateevent,
    ep.phrase,
    gereencodingnainwak(replace(replace(replace(replace(replace(replace(replace(replace(replace(replace(replace(replace(ep.phrase::text, '%casex'::text, e.ecasex::text), '%casey'::text, e.ecasey::text), '%Nain2'::text, ('<i>'::text || COALESCE(vdn2.lib_nain, ''::character varying)::text) || '</i>'::text), '%Nain'::text, ('<i>'::text || COALESCE(vdn1.lib_nain, ''::character varying)::text) || '</i>'::text), '%objet'::text, COALESCE(o.nom, ''::character varying)::text), '%monde'::text, COALESCE(m.lib, ''::text)), '%pv'::text, COALESCE(e.epv::text, ''::text)), '%gr'::text, COALESCE(e.egrad, ''::text)), '%pc'::text, e.eptcote::text), '%ph'::text, e.ehont::text), '%xp'::text, e.exp::text), '%vh'::text, COALESCE(ovh.nom, ''::character varying)::text)) AS ephrase,
    m.lib AS mondefromdetect,
    e.etype,
    o.image,
    vdn1.lib_nain AS nain1,
    'couleur Nain'::text AS couln,
    vdnub4.lib_nain AS userb4,
    (now() - e.edateevent::timestamp with time zone) < '1 day'::interval AS recent,
    e.eidmde,
    e.edateevent AS datebrut
   FROM events e
     JOIN events_phrase ep ON ep.idevent = e.etype
     LEFT JOIN v_dernier_nom_nainshisto vdn1 ON e.eidnain1 = vdn1.idnh
     LEFT JOIN v_dernier_nom_nainshisto vdn2 ON e.eidnain2 = vdn2.idnh
     JOIN utilisateur u ON e.eiddetecteur = u.id
     JOIN v_dernier_nom vdnub4 ON vdnub4.id = u.id_nain
     LEFT JOIN monde m ON e.eidmde = m.id
     LEFT JOIN objet o ON o.id = e.eobj1
     LEFT JOIN objet ovh ON ovh.id = e.evh
  WHERE e.edateevent > (now() - '5 days'::interval) AND e.edateevent < (now() + '02:00:00'::interval)
  Group By e.etype
  Having e.etype = 42
  ORDER BY m.lib, e.edateevent DESC
  Limit 50 Offset 2;";;
(*
type t =
  | String of string
  | Number of string (* float is not appropriate for decoding 64bit int *)
  | Object of obj
  | Array of t list
  | Bool of bool
  | Null
and obj = (string * t) list

 *)

(*
*)

type selectQuery =
        | WithClause of Tiny_json.Json.t
        | Select of Tiny_json.Json.t
        | From of Tiny_json.Json.t
        | Where  of Tiny_json.Json.t
        | GroupBy of Tiny_json.Json.t
        | Having of Tiny_json.Json.t
        | OrderBy of Tiny_json.Json.t
        | Limit of Tiny_json.Json.t
        | Top of Tiny_json.Json.t
        | Offset of Tiny_json.Json.t

let validJsonOfJsont  j =
        let buffer = Buffer.create 65535 in
        let formatter = Format.formatter_of_buffer buffer in
        Tiny_json.Json.format formatter j;
        Format.pp_print_flush formatter ();
        Buffer.contents buffer;;

let json2Grammar ( json : Tiny_json.Json.t)  =
        let printJsonList  = List.iter (fun elem -> validJsonOfJsont elem |> print_endline) in
        let getSelectStatement elem =
                match elem with
                | Object ( ("SelectStmt", champs )::_) -> champs 
                | _ -> failwith "SelectStmt pas trouvé" in
        let getSelectClauses elem =
                match elem with 
                | Object ( ("targetList",Array( select ))::("fromClause",Array( from ))::_) -> select, from
                | _ -> failwith "SelectStmt pas trouvé" in
        let getGoodClause j =
                match j with 
                | ("targetList",select ) -> Some(Select(select))
                | ("fromClause", from ) -> Some(From(from))
                | ("whereClause", subWhere) -> Some(Where(subWhere))
                | ("groupClause", groupClause ) -> Some(GroupBy(groupClause))
                | ("havingClause",  havingClause ) -> Some(Having(havingClause))
                | ("withClause",  Object (("ctes",   ctessub )::_ ))    -> Some(WithClause(ctessub))
                | ("sortClause", orderBy ) -> Some(OrderBy(orderBy))
                | ("limitCount",limitCount) -> Some(Limit(limitCount))
                | ("limitOffset",limitOffset) -> Some(Top(limitOffset))
                | ("TODO",select ) -> None
                | _ -> None in
        match json with
        | Object( version::("stmts",Array( stmt ) )::_   ) -> (*printJsonList stmt ;*)
                        (
                match stmt with
                | (Object ( ("stmt", Object ( ("SelectStmt", Object(clauses) )::_) )::_ ) )::[] ->
                               List.map  getGoodClause clauses
                     (* let selectStmt =  getSelectStatement selectStmt in
                      validJsonOfJsont selectStmt |> print_endline;
                      let select, from = getSelectAndFrom selectStmt in
                                printJsonList select;
                                printJsonList from;*) 
        
                | _ -> failwith "pas pas  match"
        )
        | _ -> failwith "pas pas  match"

;;

let () =
  let statement = "SELECT user, email FROM users WHERE id = 7" in
  let ast = (Pg_query.raw_parse sql2).parse_tree  in
  (*let _ = BatString.nreplace ~str:ast  ~sub:"\\" ~by:""  |> print_endline in
  let _ = print_endline ast in*)
  let json = BatString.nreplace ~str:ast  ~sub:"\\" ~by:""  |> Tiny_json.Json.parse  in
  let _ = validJsonOfJsont json |> print_endline in 
  let _ = json2Grammar json in
  ()
