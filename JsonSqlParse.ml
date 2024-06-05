open Tiny_json.Json;;

let validJsonOfJsont  j =
        let buffer = Buffer.create 65535 in
        let formatter = Format.formatter_of_buffer buffer in
        Tiny_json.Json.format formatter j;
        Format.pp_print_flush formatter ();
        Buffer.contents buffer;;



type tableName = string 
and schemaName = string
and  indexStmt =
       |  Btree of string  * schemaName * tableName * string  * ( string * string) list (* champ, opérateur, options*)
       |  Hash of string   * schemaName * tableName * string  * ( string * string) list (* champ, opérateur, options*)
       |  Gin of string    * schemaName * tableName * string  * string option * ( string * string) list (* champ, opérateur, options*)
       |  Gist of string   * schemaName * tableName * string  * ( string * string) list (* champ, opérateur, options*)
       |  Brin of string   * schemaName * tableName * string  * ( string * string) list (* champ, opérateur, options*)
       |  SpGist of string * schemaName * tableName * string  * ( string * string) list (* champ, opérateur, options*)
;;


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
        | Offset of Tiny_json.Json.t
and sqlEntry =
        | SelectStatement of selectQuery option list (*provisoir*)
        | IndexCreation of indexStmt
and scriptSql = sqlEntry list;;    

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


let json_to_indexElem idxname schemaname tblname typ elem = 

        let makeIdxElem methodName idxname shema tbl col op ordr  nullordr =
               match methodName with
                | "hash" -> Hash(idxname, shema, tbl, col, []) (*TODO option si utile*)
                | "gist" -> Gist(idxname, shema, tbl, col, [])
                | "gin" -> Gin(idxname, shema, tbl, col, Some(op), [])
                | "brin" -> Brin (idxname, shema, tbl, col, [])
                | _ -> Btree(idxname, shema, tbl, col, []) in
        match elem with 
        |  Object
             (("name", String column)::
              ("opclass",
                        Array
                         [
                                 Object [("String", Object [("str", String schemaOp)])];
                                 Object [("String", Object [("str", String operateur)])]
                          ]
                       )::
              ("ordering", String ordre)::
              ("nulls_ordering", String null_ordre)::_)
           -> makeIdxElem typ idxname schemaname tblname column operateur ordre  null_ordre
              
        | Object
            (("name", String column)::
             ("ordering", String ordre)::
             ("nulls_ordering", String null_ordre)::_)
           -> makeIdxElem typ idxname schemaname tblname column "" ordre  null_ordre
        |  json -> failwith ("Unsupported json_to_indexElem: " ^ validJsonOfJsont json);;
            
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

        | json -> failwith ("Unsupported condition join: " ^ validJsonOfJsont json)
;;



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
   (*     let getOneIndexCreation idxcreat = 
                match idxcreat with
                | ("stmt", Object [("IndexStmt",

	       (Object (
		("idxname", String idxname)::

               	("relation",
                Object
                 (("schemaname", String schemaName)::
                  ("relname", String tableName):: ("inh", _)::
                  ("relpersistence", _)::_))::

               ("accessMethod", String typeIndex)::
               ("indexParams",
                Array
                 (Object
                   (("IndexElem",  elem    )::_)::_
                 ))::_
                ))
                )]) -> IndexCreation (json_to_indexElem idxname schemaName tableName typeIndex elem) in*)
        let getOneStatement statement = 
                match statement with

                | Object ( ("stmt", Object ( ("SelectStmt", Object(clauses) )::_) )::_ )  -> SelectStatement (List.map  getGoodClause clauses)

                | Object (("stmt", Object [("IndexStmt",

	       (Object (
		("idxname", String idxname)::

               	("relation",
                Object
                 (("schemaname", String schemaName)::
                  ("relname", String tableName):: ("inh", _)::
                  ("relpersistence", _)::_))::

               ("accessMethod", String typeIndex)::
               ("indexParams",
                Array
                 (Object
                   (("IndexElem",  elem    )::_)::_
                 ))::_
                ))
                )])::_ ) -> IndexCreation (json_to_indexElem idxname schemaName tableName typeIndex elem) 
                | json -> failwith ("Unsupported getOneSelectQuery: " ^ validJsonOfJsont json) in
        match json with
        | Object( version::("stmts",Array( stmts ) )::_   ) -> (*printJsonList stmt ;*)
                        let idxParsedList = List.map getOneStatement stmts
                        in idxParsedList
                        (*(
                match stmts with
                | (Object ( ("stmt", Object ( ("SelectStmt", Object(clauses) )::_) )::_ ) )::[] ->
                               SelectStatement (List.map  getGoodClause clauses)
                     (* let selectStmt =  getSelectStatement selectStmt in
                      validJsonOfJsont selectStmt |> print_endline;
                      let select, from = getSelectAndFrom selectStmt in
                                printJsonList select;
                                printJsonList from;*) 
                |    (Object ( idxlist ))::_ -> let idxParsedList = List.map getOneIndexCreation idxlist in

        
                | _ -> failwith "pas pas  match"
        ) *)
        | _ -> failwith "pas pas  match"

;;



