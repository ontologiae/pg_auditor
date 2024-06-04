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


let sql_index = "
CREATE INDEX \"AllDetect2\" ON public.detects USING btree (didnain, ddatecreat, dest_actif, didnaindetecteur, dxpos, dypos, dmonde, didobj);
CREATE INDEX \"ObjsIdo\" ON public.objrins USING hash (ido);
CREATE INDEX avoir_nom_id_idx ON public.avoir_nom USING btree (id) INCLUDE (date_nom);
CREATE INDEX planet_osm_polygon_way_idx ON public.planet_osm_polygon USING gist (way) WITH (fillfactor='100');
CREATE INDEX trgm_idx_synonyms_complets_m1 ON public.synonyms_complets USING gin (m1 public.gin_trgm_ops);

";;
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

type
joinType =
        | Inner | FullOuter | Left | Right | Lateral | Cross
and  fromClause =
        | JoinExpre of joinType * fromClause * fromClause * condJoin
        | CondExpre of expreCond
        | Inconnu (*Grammaire non gérée*)
and op = Equal | NotEqual | Inf | InfEq | Sup | SupEq | Any | All | Like | Ilike
and condJoin = 
        | Cond of op * condJoin * condJoin
        | CondExpre of expreCond
        | NA
and expreCond =
        | TableChampRef of string * string (*table Alias*)
        | ColumnRef of string * string (*Alias.champ*)
        | SubQuery of selectQuery (**)
        | ConstStr of string
        | ConstNbr of int64
        | ConstTimestamp of string
        | ConstDate of string
and selectQuery =
        | WithClause of Tiny_json.Json.t
        | Select of Tiny_json.Json.t
        | From of fromClause * Tiny_json.Json.t (*On garde le sous arbre JSON pour le moment, on a pas encore couvert la grammaire entière*)
        | Where  of Tiny_json.Json.t
        | GroupBy of Tiny_json.Json.t
        | Having of Tiny_json.Json.t
        | OrderBy of Tiny_json.Json.t
        | Limit of Tiny_json.Json.t
        | Top of Tiny_json.Json.t
        | Offset of Tiny_json.Json.t;;



let validJsonOfJsont  j =
        let buffer = Buffer.create 65535 in
        let formatter = Format.formatter_of_buffer buffer in
        Tiny_json.Json.format formatter j;
        Format.pp_print_flush formatter ();
        Buffer.contents buffer;;

let string_to_join_type = function
    | "JOIN_INNER" -> Inner
    | "JOIN_FULL" -> FullOuter
    | "JOIN_LEFT" -> Left
    | "JOIN_RIGHT" -> Right
    | "JOIN_LATERAL" -> Lateral
    | "JOIN_CROSS" -> Cross
    | _ -> failwith "Unsupported join type";;


let string_to_op = function (*TODO Uppercase*)
    | "=" -> Equal
    | "<>" -> NotEqual
    | "<" -> Inf
    | "<=" -> InfEq
    | ">" -> Sup
    | ">=" -> SupEq
    | "ANY" -> Any
    | "ALL" -> All
    | "LIKE" -> Like
    | "ILIKE" -> Ilike
    | _ -> failwith "Unsupported operator";;


let rec json_to_expreCond = function
    | Object [("ColumnRef", Object (("fields", Array [Object [("String", Object [("str", String alias)])];
                                                                         Object [("String", Object [("str", String column)])]
                                                     ])::_
                                   ))] ->
        ColumnRef (alias, column)
   
    | Object [("A_Const", Object [("val", Object [("String", Object [("str", String s)])])])] ->
        ConstStr s

    | Object [("A_Const", Object [("val", Object [("Integer", Number n)])])] ->
        ConstNbr (Int64.of_string n)

    | Object [("A_Const", Object [("val", Object [("Float", Number n)])])] ->
        ConstNbr (Int64.of_float (float_of_string n))

    | json -> failwith ("Unsupported expression condition: " ^ validJsonOfJsont json);;


let rec json_to_condJoin = function
    | Object [("A_Expr", Object (("kind", String "AEXPR_OP")::
                                           ("name", Array [Object [("String", Object [("str", String op)])]])::
                                           ("lexpr", lexpr)::
                                           ("rexpr", rexpr)::_
                                ))] ->
        let op = string_to_op op in
        let lexpr = json_to_expreCond lexpr in
        let rexpr = json_to_expreCond rexpr in
         (* Placeholder for pattern matching, replace with actual condition parsing *)
        Cond(op,CondExpre lexpr, CondExpre rexpr)
    | json -> failwith ("Unsupported condition join: " ^ validJsonOfJsont json);;



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
                | ("fromClause", from ) -> Some(From(Inconnu,from))
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


let rec json_to_fromClause clause =
        let rec array_to_JoinExpreCross l =
                match l with
                | t::t2::[]     -> JoinExpre(Cross,json_to_fromClause t,json_to_fromClause t2, NA)                
                | t::q          -> JoinExpre(Cross,json_to_fromClause t, array_to_JoinExpreCross q, NA)
                | [] -> failwith "array_to_JoinExpreCross []" in

        match clause with  
        | Object [("JoinExpr", Object [("jointype", String jointype);
                                             ("larg", larg);
                                             ("rarg", rarg);
                                             ("quals", quals)])] ->
                                                     let join_type = string_to_join_type jointype in
                                                     let larg = json_to_fromClause larg in
                                                     let rarg = json_to_fromClause rarg in
                                                     let quals = json_to_condJoin quals in
                                                     JoinExpre (join_type, larg, rarg, quals)
        | Array (t::[]) -> json_to_fromClause t
        | Array l -> array_to_JoinExpreCross l
        | Object [("RangeVar", Object  (("relname", String n)::("inh", Bool _)::("relpersistence", String _)::("alias", Object [("aliasname", String alias)])::_))]
         -> CondExpre(TableChampRef(n,alias))
        | json -> failwith ("Unsupported condition join: " ^ validJsonOfJsont json);;






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
                | ("fromClause", from ) -> Some(From(Inconnu,from))
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
  let ast = (Pg_query.raw_parse sql_index).parse_tree  in
  (*let _ = BatString.nreplace ~str:ast  ~sub:"\\" ~by:""  |> print_endline in
  let _ = print_endline ast in*)
  let json = BatString.nreplace ~str:ast  ~sub:"\\" ~by:""  |> Tiny_json.Json.parse  in
  let _ = validJsonOfJsont json |> print_endline in 
  let _ = json2Grammar json in
  ()
