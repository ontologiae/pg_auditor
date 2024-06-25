open Tiny_json.Json;;

module L = List;;

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
        | Inner | FullOuter | Left | Right | Lateral | Cross | Natural
and  fromClause =
        | JoinExpre of joinType * fromClause * fromClause * condJoin
        | CondExpre of expreCondTerm
        | SubQuery of sqlEntry
        | Inconnu (*Grammaire non gérée*)
and op = And | Or | Equal | NotEqual | Inf | InfEq | Sup | SupEq | Any | All | Like | Ilike | Between | In
and condJoin = 
        | Cond of op * condJoin * condJoin
        | CondExpre of expreCondTerm
        | NA
and whereClause =
        | AndBoolExpre of whereClause * whereClause
        | OrBoolExpre of whereClause * whereClause
        | InBoolExpre of expreCondTerm * expreCondTerm
        | IsNullExpre of expreCondTerm
        | IsNotNullExpre of expreCondTerm
        | IsTrueOrNot of bool * expreCondTerm
        | NotBoolExpre of whereClause
        | AllBoolExpre of whereClause
        | AnyBoolExpre of whereClause
        | SomeBoolExpre of whereClause
        | ExistsBoolExpre of selectQuery
        | Between of expreCondTerm * expreCondTerm
        | LikeBoolExpre of expreCondTerm * expreCondTerm
        | WhereCondExpre of condWhere
        | SubQuery of selectQuery
and condWhere =
        | OpCond of op * expreCondTerm * expreCondTerm
        | FunctionCall of string * expreCondTerm list (*arguments de la fonction*)
and expreCondTerm =
        | TableChampRef of string * string (*table Alias*)
        | TableName of string
        | FunctionCall of string * expreCondTerm list (*arguments de la fonction*)
        | ColumnRef of string option * string (*Alias.champ*)
        | SubQuery of selectQuery (**)
        | ConstStr of string
        | ConstNbr of int64
        | ConstTimestamp of string
        | ConstDate of string
        | TypeCast of string * expreCondTerm
        | ListTerm of expreCondTerm list
        | ParamRef of int64
        | ArgStarAll
        | ConstNull
and selectQuery =
        | WithClauses of ( Tiny_json.Json.t * tableName * sqlEntry) list
        | Select of Tiny_json.Json.t
        | From of fromClause * Tiny_json.Json.t (*On garde le sous arbre JSON pour le moment, on a pas encore couvert la grammaire entière*)
        | Where  of  whereClause * Tiny_json.Json.t
        | GroupBy of Tiny_json.Json.t
        | Having of whereClause * Tiny_json.Json.t
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


let string_to_op op =  (*TODO Uppercase*)
        match op with
    | "=" -> Equal
    | "<>" -> NotEqual
    | "u003c" -> Inf
    | "<" -> Inf
    | "<=" -> InfEq
    | "u003e" -> Sup
    | ">" -> Sup
    | "u003e="  -> SupEq
    | "u003e"   -> Sup  
    | ">=" -> SupEq
    | "ANY" -> Any
    | "ALL" -> All
    | "LIKE" -> Like
    | "ILIKE" -> Ilike
    | "~~" -> Like
    | "BETWEEN" -> Between
    | "IN" -> In
    | _ -> failwith ("Unsupported operator : "^op);;



let rec jsonToWhereClause json =
        match json with
        |    Object (("BoolExpr", Object (("boolop", String "AND_EXPR")::("args", Array l)::_))::_)  ->  l
        |    Object (("BoolExpr", Object (("boolop", String "OR_EXPR")::("args", Array l)::_))::_)   -> l



(* Helper function to parse a JSON string to int64 *)
let int64_of_json_string json = match json with
                                | Number u -> Int64.of_string u
                                | _ -> failwith ("Unsupported int64_of_json_string: " ^ validJsonOfJsont json)

(* Helper function to extract a string from JSON *)
let extract_string json = match json with
  | String s -> s
  | _ -> failwith "Expected a JSON string"

(*TODO fusionner avec parse_expreCondTerm ?*)
(*   - : t -> expreCondTerm *)
let rec json_to_expreCond = function
    | Object [("ColumnRef", Object (("fields", Array [Object [("String", Object [("str", String alias)])];
                                                                         Object [("String", Object [("str", String column)])]
                                                     ])::_
                                   ))] ->
        ColumnRef (Some(alias), column)
   
    | Object [("A_Const", Object ((("val", Object [("String", Object [("str", String s)])]))::_))] ->
        ConstStr s

    | Object [("A_Const", Object ((("val", Object [("Integer", Object [("ival", Number n)] )]))::_)  )] ->
        ConstNbr (Int64.of_string n)

    | Object [("A_Const", Object ((("val", Object [("Float", Object [("str", String n)])]))::_))] ->
        ConstNbr (Int64.of_float (float_of_string n))

    | json -> failwith ("Unsupported expression condition: " ^ validJsonOfJsont json);;




(*  parse expreCondTerm
- : t -> expreCondTerm = <fun>

*)
let rec parse_expreCondTerm json =
        let getTypeName json =
                match json with
                | Object (("names",  Array  [Object [("String", Object [("str", String schemat)])];
                                             Object [("String", Object [("str", String typen)])]]
                          )::_) -> schemat ^"."^typen
                | Object (("names", Array (Object [("String", Object [("str", String typenam)])] ::_) )::_ ) -> typenam
                | _ -> failwith ("Unsupported getTypeName: " ^ validJsonOfJsont json) in
  match json with
  | Object (("ColumnRef", Object (("fields", Array fields)::("location", _)::_))::_) ->
      (match fields with
       | [Object [("String", Object [("str", String table)])];
          Object [("String", Object [("str", String column)])]] ->
         ColumnRef (Some(table), column)
       | [Object [("String", Object [("str", String column)])]] -> ColumnRef (None, column)
       | _ -> let jsonprint = L.map validJsonOfJsont fields |> String.concat "\n" in
                let _ = print_endline jsonprint in
                failwith ("Unrecognized ColumnRef structure: " ))
      
  | Object [("FuncCall", Object [("funcname", Array [Object [("String", Object [("str", String fname)])]]); ("location", _)])] ->
      FunctionCall (fname, [])

  | Object [("FuncCall", Object [("funcname", Array [Object [("String", Object [("str", String fname)])]]); ("args",Array l); ("location", _)])] ->
      FunctionCall (fname, L.map parse_expreCondTerm l)

  | Object [("FuncCall", Object (("funcname", Array [Object [("String", Object [("str", String fname)])]]) :: ("agg_star",Bool true)::_ ))] ->
                  FunctionCall (fname, [ArgStarAll])

  | Object [("A_Expr", Object [("kind", String "AEXPR_OP"); ("name", Array [Object [("String", Object [("str", String op)])]]); ("lexpr", lexpr); ("rexpr", rexpr); ("location", _)])] ->
      let lexpr_parsed = parse_expreCondTerm lexpr in
      let rexpr_parsed = parse_expreCondTerm rexpr in
      (* Placeholder for now based on the specific structure *)
      FunctionCall (op, [lexpr_parsed; rexpr_parsed]) (*TODO : c'est as du tout un funcall !!*)

  | Object (("TypeCast", Object (("arg", arg)::("typeName", typename)::_))::_) ->
      let arg_parsed = parse_expreCondTerm arg in
      TypeCast (getTypeName typename, arg_parsed)

  | Object [("A_Const", Object (("val", Object ( ("String", Object [("str", String str)] )::_ ))::_ ))] ->
      ConstStr str
  | Object [("A_Const", Object (("val", _ )::_ ))] ->
      json_to_expreCond json
  |   Object [("List",  Object [("items",  Array l )])] -> ListTerm ( L.filter_map (fun e -> match e with Object [] -> None | _ -> Some(parse_expreCondTerm e)) l  )

  |   Object (("ParamRef", Object (("number", Number nbr)::_))::_) -> ParamRef (Int64.of_string nbr)

  | _ -> let jsonprint = validJsonOfJsont json in
                let _ = print_endline jsonprint in
                failwith ("Unsupported parse_expreCondTerm: " ^ validJsonOfJsont json)




(*  parse whereClause BoolExpr 
- : t -> whereClause = <fun>
 *)
let rec parse_whereClause json =
  match json with
  (* AND boolean expression *)
  | Object [("BoolExpr", Object [("boolop", String "AND_EXPR"); ("args", Array [arg1; arg2]); ("location", _)])] ->
      let arg1_parsed = parse_whereClause arg1 in 
      let arg2_parsed = parse_whereClause arg2 in
      AndBoolExpre (arg1_parsed, arg2_parsed)
  | Object [("BoolExpr", Object [("boolop", String "OR_EXPR"); ("args", Array [arg1; arg2]); ("location", _)])] ->
      let arg1_parsed = parse_whereClause arg1 in 
      let arg2_parsed = parse_whereClause arg2 in
      OrBoolExpre (arg1_parsed, arg2_parsed)
      
  | Object   [("BoolExpr",      Object      (("boolop", String "AND_EXPR")::("args",Array (arg1::q) )::_))] ->
                  L.fold_left ( fun a -> fun b -> AndBoolExpre(  a, parse_whereClause b) ) (parse_whereClause arg1) q
  | Object   [("BoolExpr",      Object      (("boolop", String "OR_EXPR")::("args",Array (arg1::q) )::_))] ->
                  L.fold_left ( fun a -> fun b -> OrBoolExpre(  a, parse_whereClause b) ) (parse_whereClause arg1) q

  | Object [("A_Expr", Object [("kind", String "AEXPR_OP"); ("name", Array [Object [("String", Object [("str", String op)])]]); ("lexpr", lexpr); ("rexpr", rexpr); ("location", _)])] ->
      let lexpr_parsed = parse_expreCondTerm lexpr in
      let rexpr_parsed = parse_expreCondTerm rexpr in
        WhereCondExpre(OpCond(string_to_op op,lexpr_parsed, rexpr_parsed ))
  | Object [("A_Expr", Object [("kind", String "AEXPR_BETWEEN"); ("name", Array [Object [("String", Object [("str", String op)])]]); ("lexpr", lexpr); ("rexpr", rexpr); ("location", _)])] ->
      let lexpr_parsed = parse_expreCondTerm lexpr in
      let rexpr_parsed = parse_expreCondTerm rexpr in
        WhereCondExpre(OpCond(string_to_op op,lexpr_parsed, rexpr_parsed ))
  | Object [("A_Expr", Object [("kind", String "AEXPR_IN"); ("name", Array [Object [("String", Object [("str", String op)])]]); ("lexpr", lexpr); ("rexpr", rexpr); ("location", _)])] ->
      let lexpr_parsed = parse_expreCondTerm lexpr in
      let rexpr_parsed = parse_expreCondTerm rexpr in
        WhereCondExpre(OpCond(In,lexpr_parsed, rexpr_parsed ))
 | Object [("A_Expr", Object [("kind", String "AEXPR_LIKE"); ("name", Array [Object [("String", Object [("str", String op)])]]); ("lexpr", lexpr); ("rexpr", rexpr); ("location", _)])] ->
      let lexpr_parsed = parse_expreCondTerm lexpr in
      let rexpr_parsed = parse_expreCondTerm rexpr in
        WhereCondExpre(OpCond(Like,lexpr_parsed, rexpr_parsed ))
        
  | Object  (("NullTest",  Object (("arg", arg)::_))::_) -> IsNullExpre( parse_expreCondTerm arg)

  | Object (("ColumnRef", colref)::_) -> IsTrueOrNot(true, parse_expreCondTerm json)
  


  | _ -> print_endline (validJsonOfJsont json);
         failwith ("Unknown whereClause structure: " ^ validJsonOfJsont json) 



  

(*  - : t -> condJoin   

 TODO faut mettre des try catch pour lancer json_to_condJoin au cas où on a des subexpre*)
let rec json_to_condJoin json : condJoin =
        let trySubCondJoin json =
                try json_to_condJoin json with e -> CondExpre(parse_expreCondTerm json) in
   match json with 
    | Object [("A_Expr", Object (("kind", String "AEXPR_OP")::
                                           ("name", Array [Object [("String", Object [("str", String op)])]])::
                                           ("lexpr", lexpr)::
                                           ("rexpr", rexpr)::_
                                ))] ->
        let op = string_to_op op in
        let lexpr = trySubCondJoin lexpr in
        let rexpr = trySubCondJoin rexpr in
         (* Placeholder for pattern matching, replace with actual condition parsing *)
        Cond(op, lexpr,  rexpr)
    | Object   [("BoolExpr",      Object      (("boolop", String "AND_EXPR")::("args",Array (arg1::arg2::[]) )::_))] ->
                   let lexpr = trySubCondJoin arg1 in
                   let rexpr = trySubCondJoin arg2 in
                    Cond(And,  lexpr,  rexpr)
                    
    | Object   [("BoolExpr",      Object      (("boolop", String "OR_EXPR")::("args",Array (arg1::arg2::[]) )::_))] ->
                   let lexpr = trySubCondJoin arg1 in
                   let rexpr = trySubCondJoin arg2 in
                    Cond(Or,  lexpr,  rexpr)
                    
    | Object   [("BoolExpr",      Object      (("boolop", String "AND_EXPR")::("args",Array (arg1::q) )::_))] ->
                    L.fold_left ( fun a -> fun b -> Cond(And,  a, json_to_condJoin b) ) (json_to_condJoin arg1) q

    | Object   [("BoolExpr",      Object      (("boolop", String "Or_EXPR")::("args",Array (arg1::q) )::_))] ->
                    L.fold_left ( fun a -> fun b -> Cond(Or,  a, json_to_condJoin b) ) (json_to_condJoin arg1) q
                    

    | json -> failwith ("Unsupported condition join: " ^ validJsonOfJsont json);;







(*  - : string -> string -> string -> string -> t -> indexStmt *)
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
           

(*- : t -> fromClause*)
let rec json_to_fromClause clause =
        (*TODO  f108 f110 f111 f113*)
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
         | Object [("JoinExpr", Object (("jointype", String jointype):: (*Cas subtil d'inner sans condition : Lateral*)
                                             ("larg", larg)::
                                             ("rarg", Object (("RangeFunction", Object (("lateral", Bool true)::("functions",Array funcs)::("alias", Object [("aliasname", String aliasname)] )::_))::_ )
                                             )::_
         )
                    )
                   ]  ->
                                                     let join_type = string_to_join_type jointype in
                                                     let larg = json_to_fromClause larg in
                                                     let rarg = L.hd funcs |> parse_expreCondTerm  in
                                                     JoinExpre (Lateral, larg, CondExpre rarg, NA)


         | Object [("JoinExpr", Object (("jointype", String jointype):: (*Cas subtil d'inner sans condition : Lateral*)
                                             ("larg", larg)::
                                             ("rarg", Object (("RangeSubselect", Object (("lateral", Bool true)::("subquery",selectStmt)::("alias", Object [("aliasname", String aliasname)] )::_))::_ )
                                             )::_
         )
                    )
                   ]  ->
                                                     let join_type = string_to_join_type jointype in
                                                     let larg = json_to_fromClause larg in
                                                     let rarg = getOneStatement selectStmt  in
                                                     JoinExpre (Lateral, larg, SubQuery rarg, NA)
                                                     

         | Object (("JoinExpr", Object (("jointype", String jointype)::("isNatural", Bool true)::
                                             ("larg", larg)::
                                             ("rarg", rarg)::_ ))::_) ->
                                                     let join_type = string_to_join_type jointype in
                                                     let larg = json_to_fromClause larg in
                                                     let rarg = json_to_fromClause rarg in
                                                     JoinExpre (Natural, larg, rarg, NA)

                                                     
        | Array (t::[]) -> json_to_fromClause t

        | Array l -> array_to_JoinExpreCross l

        | Object [("RangeVar", Object  (("relname", String n)::("inh", Bool heritage)::("relpersistence", String persist)::("alias", Object [("aliasname", String alias)])::_))]
         -> CondExpre(TableChampRef(n,alias))

        | Object [("RangeVar", Object  (("relname", String n)::("inh", Bool heritage)::("relpersistence", String persist)::_))]
        -> CondExpre(TableName(n))


        |  Object   (("RangeSubselect", Object (("subquery", subquery)::_))::_) -> SubQuery ( getOneStatement subquery  ) (*Il est normalement impossible qu'il y ait 2 subquery*)


        | json -> let jsonprint = validJsonOfJsont json in
                let _ = print_endline jsonprint in
                failwith ("Unsupported json_to_fromClause: " ^ validJsonOfJsont json)

and parseWithClause withClause = 
        match withClause with
        |  Object  [("CommonTableExpr", 
                        Object ( ("ctename", String ctename)::("ctematerialized", String cTEMaterializeDefault_param)::
                                 ("ctequery",  selectStmt )::_
                               )
                    )
                   ] ->   Null, ctename, getOneStatement selectStmt
        |  Object  [("CommonTableExpr",                         (*Joker*) 
                        Object ( ("ctename", String ctename)::(_,_)::("ctematerialized", String cTEMaterializeDefault_param)::
                                 ("ctequery",  selectStmt )::_
                               )
                    )
                   ] ->   Null, ctename, getOneStatement selectStmt

        | _ ->  let jsonprint = validJsonOfJsont withClause in
                let _ = print_endline jsonprint in
                failwith ("Unsupported parseWithClause: " ^ validJsonOfJsont withClause)



(*    : string * t -> selectQuery option *)
and  getGoodClause j =
                match j with 
                | ("targetList",select ) -> Some(Select(Null))
                | ("fromClause", from ) -> Some(From(json_to_fromClause from,Null))
                | ("whereClause", subWhere) -> Some(Where(parse_whereClause subWhere,  Null))
                | ("groupClause", groupClause ) -> Some(GroupBy(groupClause))
                | ("havingClause",  havingClause ) -> Some(Having(parse_whereClause havingClause, Null))
                | ("withClause",  Object (("ctes",  Array ctessubs )::_ ))  -> Some( WithClauses (L.map parseWithClause ctessubs ))
                | ("sortClause", orderBy ) -> Some(OrderBy(orderBy))
                | ("limitCount",limitCount) -> Some(Limit(limitCount))
                | ("limitOffset",limitOffset) -> Some(Top(limitOffset))
                | ("TODO",select ) -> None
                | _ -> None

and getOneStatement statement = 
                match statement with

                | Object ( ("stmt", Object ( ("SelectStmt", Object(clauses) )::_) )::_ )  -> SelectStatement (L.map  getGoodClause clauses)
                | Object ( ("SelectStmt", Object(clauses) )::_) -> SelectStatement (L.map  getGoodClause clauses)

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
                                Array (Object        
                                   (("IndexElem",  elem    )::_)::_
                                   ))::_
                                                 ))
                        )])::_ ) -> IndexCreation (json_to_indexElem idxname schemaName tableName typeIndex elem) 

                | json -> failwith ("Unsupported getOneSelectQuery: " ^ validJsonOfJsont json)

and  json2Grammar ( json : Tiny_json.Json.t) :  sqlEntry list =
        let printJsonList  = L.iter (fun elem -> validJsonOfJsont elem |> print_endline) in
                match json with
        | Object( version::("stmts",Array( stmts ) )::_   ) -> (*printJsonList stmt ;*)
                        let idxParsedList = L.map getOneStatement stmts
                        in idxParsedList
        | _ -> failwith "pas pas  match"

;;



