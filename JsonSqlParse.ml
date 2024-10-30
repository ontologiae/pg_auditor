open Tiny_json.Json;;
open Jsonm

module L = List;;



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

(*
 j/m74.json et 85
 objet avec litéraux dedans

m86 et 88: monstre qui fail
m187 monstre where
COALESCE 162
 *)
type
joinType =
        | Inner | FullOuter | Left | Right | Lateral | Cross | Natural 
and  fromClause =
        | JoinExpre of joinType * fromClause * fromClause * condJoin
        | FromExpre of expreCondTerm
        | FromSubQuery of sqlEntry
        | Inconnu (*Grammaire non gérée*)
and op = And | Or | Equal | NotEqual | Inf | InfEq | Sup | SupEq | Any | All | Like | Ilike | Between | In
and condJoin = 
        | Cond of op * condJoin * condJoin
        | CondExpre of expreCondTerm
        | NA

      (*Est-ce qu'en fait on derait pas regrouper la plupart sous boolExpreTerm et définir whereClause
       comme boolExpreTerm * op * boolExpreTerm list*)  
and whereClause =
        | AndBoolExpre of whereClause * whereClause
        | OrBoolExpre of whereClause * whereClause
        | InBoolExpre of expreCondTerm * expreCondTerm
        | IsNullExpre of expreCondTerm
        | IsNotNullExpre of expreCondTerm
        | IsTrueOrNot of bool * expreCondTerm
        | BoolConst of bool
        | NotBoolExpre of whereClause
        | AllBoolExpre of whereClause
        | AnyBoolExpre of whereClause
        | SomeBoolExpre of whereClause
        | ExistsBoolExpre of selectQuery
        | Between of expreCondTerm * expreCondTerm
        | LikeBoolExpre of expreCondTerm * expreCondTerm
        | WhereCondExpre of condWhere
        | WhereSubQuery of selectQuery * op
and condWhere =
        | OpCond of op * expreCondTerm * expreCondTerm
        | FunctionCall of string * expreCondTerm list (*arguments de la fonction*)
        (*TODO : un expreCondTerm devrait exprimer un booléen, pas n'importe quoi : un TableName ne peut être un booléen exemple avec j/m54.json  108 où on a un null test qui tombe dans un where*)
and expreCondTerm = 
        | TableChampRef of string option * string * string (*table schema.name Alias*)
        | TableName of string option * string (*table schema.name TODO : pas une expression*)
        | FunctionCall of string * expreCondTerm list (*arguments de la fonction*)
        | ColumnRef of string option * string (*Alias.champ*)
        | CoalesceExpr of expreCondTerm list
        | ExpreSubQuery of sqlEntry * op (**)
        | ConstStr of string
        | ConstNbr of int64
        | ConstTimestamp of string
        | ConstDate of string
        | ConstBool of bool
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
    | "u003cu003e" -> NotEqual
    | "<>" -> NotEqual
    | "u003c" -> Inf
    | "<" -> Inf
    | "<=" -> InfEq
    | "u003c="  -> InfEq
    | "u003c"   -> Inf 
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



let objectExists (objs : (string * Tiny_json.Json.t) list)  name =
        L.mem_assoc name objs ;;

let rec jsonToWhereClause json =
        match json with
        |    Object (("BoolExpr", Object (("boolop", String "AND_EXPR")::("args", Array l)::_))::_)  ->  l
        |    Object (("BoolExpr", Object (("boolop", String "OR_EXPR")::("args", Array l)::_))::_)   -> l




(* Helper function to extract a string from JSON *)
let extract_string json = match json with
  | String s -> s
  | _ -> failwith "Expected a JSON string";;



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


(*TODO fusionner avec parse_expreCondTerm ?
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


*)

let op_of_sublink_op operator =
        match operator with
        | "EXPR_SUBLINK" -> Equal
        | "ANY_SUBLINK"  -> In;;





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
                let _ = prerr_endline jsonprint in
                failwith ("Unrecognized ColumnRef structure: " ))
      
  | Object [("FuncCall", Object [("funcname", Array [Object [("String", Object [("str", String fname)])]]); ("location", _)])] ->
      FunctionCall (fname, [])
   | Object [("FuncCall", Object (("funcname", Array [Object [("String", Object [("str", String prefix)])];Object[("String", Object [("str", String fname)]) ] ])::_ ) )] ->
      FunctionCall (prefix^"."^fname, [])

  | Object [("FuncCall", Object [("funcname", Array [Object [("String", Object [("str", String prefix)])];Object[("String", Object [("str", String fname)]) ] ]); ("args",Array l); ("location", _)])] ->
      FunctionCall (prefix^"."^fname, L.map parse_expreCondTerm l)

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

  | Object [("A_Const", Object (("val", Object [("Null", Object [])])::_))] -> ConstNull
  | Object [("List",  Object [("items",  Array l )])] -> ListTerm ( L.filter_map (fun e -> match e with Object [] -> None | _ -> Some(parse_expreCondTerm e)) l  )

  | Object (("ParamRef", Object (("number", Number nbr)::_))::_) -> ParamRef (float_of_string nbr |> Int64.of_float)
  | Object [("ParamRef", Object [("location", _)])] -> ParamRef 0L

  | Object
         [("SubLink",
           Object
            [("subLinkType", String ope);
             ("subselect", selectStmt);("location", _)
            ])
          ]    -> (* Sous requête Select From Where 
                        "EXPR_SUBLINK" = 
                                ANY_SUBLINK IN
                        
                        *)
                let op =  op_of_sublink_op ope in
                ExpreSubQuery(getOneStatement  selectStmt,  op)
     

    | Object [("ColumnRef", Object (("fields", Array [Object [("String", Object [("str", String alias)])];
                                                                         Object [("String", Object [("str", String column)])]
                                                     ])::_
                                   ))] ->
        ColumnRef (Some(alias), column)
   
    | Object [("A_Const", Object ((("val", Object [("String", Object [("str", String s)])]))::_))] ->
        ConstStr s

    | Object [("A_Const", Object ((("val", Object [("Integer", Object [("ival", Number n)] )]))::_)  )] ->
        ConstNbr (float_of_string n |> Int64.of_float)

    | Object [("A_Const", Object ((("val", Object [("Float", Object [("str", String n)])]))::_))] ->
        ConstNbr (Int64.of_float (float_of_string n))

    |  Object (("CoalesceExpr", Object (("args", Array args)::_))::_) -> CoalesceExpr(L.map parse_expreCondTerm args)

  | _ -> let jsonprint = validJsonOfJsont json in
                let _ = prerr_endline jsonprint in
                failwith ("Unsupported parse_expreCondTerm: " ^ validJsonOfJsont json)




(*  parse whereClause BoolExpr 
- : t -> whereClause = <fun>
 *)
and  parse_whereClause json =

  match json with
  (* AND boolean expression *)
  | Object [("BoolExpr", Object [("boolop", String "AND_EXPR"); ("args", Array [arg1; arg2]); ("location", _)])] ->
      let arg1_parsed = parse_whereClause arg1 in 
      let arg2_parsed = parse_whereClause arg2 in
      AndBoolExpre (arg1_parsed, arg2_parsed)
   | Object [("BoolExpr", Object [("boolop", String "AND_EXPR"); ("args", Array [arg1; arg2; arg3]); ("location", _)])] ->
      let arg1_parsed = parse_whereClause arg1 in 
      let arg2_parsed = parse_whereClause arg2 in
      let arg3_parsed = parse_whereClause arg3 in
      AndBoolExpre(AndBoolExpre (arg1_parsed, arg2_parsed), arg3_parsed)
      (*TODO Va falloir faire un fold...*)
   | Object [("BoolExpr", Object [("boolop", String "AND_EXPR"); ("args", Array [arg1; arg2; arg3; arg4]); ("location", _)])] ->
      let arg1_parsed = parse_whereClause arg1 in 
      let arg2_parsed = parse_whereClause arg2 in
      let arg3_parsed = parse_whereClause arg3 in
      let arg4_parsed = parse_whereClause arg4 in
      AndBoolExpre(AndBoolExpre(AndBoolExpre (arg1_parsed, arg2_parsed), arg3_parsed),arg4_parsed)

  | Object [("BoolExpr", Object [("boolop", String "OR_EXPR"); ("args", Array [arg1; arg2]); ("location", _)])] ->
      let arg1_parsed = parse_whereClause arg1 in 
      let arg2_parsed = parse_whereClause arg2 in
      OrBoolExpre (arg1_parsed, arg2_parsed)
      
  | Object   [("BoolExpr",      Object      (("boolop", String "AND_EXPR")::("args",Array (arg1::q) )::_))] ->
                  L.fold_left ( fun a -> fun b -> AndBoolExpre(  a, parse_whereClause b) ) (parse_whereClause arg1) q


  | Object   [("BoolExpr",      Object      (("boolop", String "OR_EXPR")::("args",Array (arg1::q) )::_))] ->
                  (*let try_where_clause e = try parse_whereClause e with er -> TODO soit on fixe la grammaire, soit gère le cas via try catch*)
                  L.fold_left ( fun a -> fun b -> OrBoolExpre(  a, parse_whereClause b) ) (parse_whereClause arg1) q

  | Object   [("BoolExpr",      Object      (("boolop", String "OR_EXPR")::("args",Array (arg1::q) )::_))] ->
                  (*let try_where_clause e = try parse_whereClause e with er -> TODO soit on fixe la grammaire, soit gère le cas via try catch*)
                  L.fold_left ( fun a -> fun b -> OrBoolExpre(  a, parse_whereClause b) ) (parse_whereClause arg1) q


| Object [("BoolExpr", Object [("boolop", String "NOT_EXPR"); ("args", Array [arg1]); ("location", _)])] ->
      let arg1_parsed = parse_whereClause arg1 in 
      NotBoolExpre (arg1_parsed)
                  
  | Object
   [("BooleanTest",
     Object
      [("arg",
        Object
         [("ColumnRef",
           Object
            [("fields",
              Array
               [Object [("String", Object [("str", String alias)])];
                Object [("String", Object [("str", String field)])]]);
             ("location", _)]
             )]);
             ("booltesttype", String is_true_or_false); ("location", _)
         ]
       )
      ] -> let trueorfalse = if is_true_or_false = "IS_TRUE" then true else false in IsTrueOrNot(trueorfalse, ColumnRef (Some(alias), field))
  

  | Object [("A_Expr", Object [("kind", String "AEXPR_OP"); ("name", Array [Object [("String", Object [("str", String op)])]]); ("lexpr", lexpr); ("rexpr", rexpr); ("location", _)])] ->
      let lexpr_parsed = parse_expreCondTerm lexpr in
      let rexpr_parsed = parse_expreCondTerm rexpr in
        WhereCondExpre(OpCond(string_to_op op,lexpr_parsed, rexpr_parsed ))
  | Object [("A_Expr", Object [("kind", String "AEXPR_BETWEEN"); ("name", Array [Object [("String", Object [("str", String op)])]]); ("lexpr", lexpr); ("rexpr", rexpr); ("location", _)])] ->
      let lexpr_parsed = parse_expreCondTerm lexpr in
      let rexpr_parsed = parse_expreCondTerm rexpr in
        WhereCondExpre(OpCond(string_to_op op,lexpr_parsed, rexpr_parsed ))
  | Object [("A_Expr", Object [("kind", String "AEXPR_IN"); ("name", Array [Object [("String", Object [("str", String _)])]]); ("lexpr", lexpr); ("rexpr", rexpr); ("location", _)])] ->
      let lexpr_parsed = parse_expreCondTerm lexpr in
      let rexpr_parsed = parse_expreCondTerm rexpr in
        WhereCondExpre(OpCond(In,lexpr_parsed, rexpr_parsed ))
 | Object [("A_Expr", Object [("kind", String "AEXPR_LIKE"); ("name", Array [Object [("String", Object [("str", String _)])]]); ("lexpr", lexpr); ("rexpr", rexpr); ("location", _)])] ->
      let lexpr_parsed = parse_expreCondTerm lexpr in
      let rexpr_parsed = parse_expreCondTerm rexpr in
        WhereCondExpre(OpCond(Like,lexpr_parsed, rexpr_parsed ))

  | Object [("A_Expr", Object [("kind", String "AEXPR_ILIKE"); ("name", Array [Object [("String", Object [("str", String _)])]]); ("lexpr", lexpr); ("rexpr", rexpr); ("location", _)])] ->
      let lexpr_parsed = parse_expreCondTerm lexpr in
      let rexpr_parsed = parse_expreCondTerm rexpr in
        WhereCondExpre(OpCond(Ilike,lexpr_parsed, rexpr_parsed ))


  | Object [("A_Expr", Object [("kind", String "AEXPR_OP_ANY"); ("name", Array [Object [("String", Object [("str", String _)])]]); ("lexpr", lexpr); ("rexpr", rexpr); ("location", _)])] ->
      let lexpr_parsed = parse_expreCondTerm lexpr in
      let rexpr_parsed = parse_expreCondTerm rexpr in
        WhereCondExpre(OpCond(Any,lexpr_parsed, rexpr_parsed ))

        
  | Object  (("NullTest",  Object (("arg", arg)::_))::_) -> IsNullExpre( parse_expreCondTerm arg)

  | Object (("ColumnRef", colref)::_) -> IsTrueOrNot(true, parse_expreCondTerm json)

  (*genre AND FALSE ou AND TRUE*)
  | Object
    [("TypeCast",
      Object
       [("arg",
         Object
          [("A_Const",
            Object
             [("val", Object [("String", Object [("str", String falseortrue)])]);
              ("location", _)])]);
        ("typeName",
         Object
          [("names",
            Array
             [Object [("String", Object [("str", String "pg_catalog")])];
              Object [("String", Object [("str", String "bool")])]]);
           ("typemod", _); ("location", _)]);
        ("location", _)])] -> let trueorfalse =  if falseortrue = "f" then false else true in
                                BoolConst trueorfalse


  |  Object
        [("SubLink",
          Object
           (("subLinkType", String op)::
            ("testexpr", columnref)::
              ("subselect",   subQuery )::_
            )
           )       
        ] -> let ope =  op_of_sublink_op op in
                WhereCondExpre(OpCond(op_of_sublink_op op, parse_expreCondTerm columnref,  ExpreSubQuery(getOneStatement  subQuery,  ope) ) )
            
  | _ -> prerr_endline (validJsonOfJsont json);
              failwith ("Unknown whereClause structure: " ^ validJsonOfJsont json) 
     


  

(*  - : t -> condJoin   

 TODO faut mettre des try catch pour lancer json_to_condJoin au cas où on a des subexpre*)
and json_to_condJoin json : condJoin =
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
                    L.fold_left ( fun a -> fun b -> Cond(And,  a, trySubCondJoin b) ) (trySubCondJoin arg1) q

    | Object   [("BoolExpr",      Object      (("boolop", String "Or_EXPR")::("args",Array (arg1::q) )::_))] ->
                    L.fold_left ( fun a -> fun b -> Cond(Or,  a, trySubCondJoin b) ) (trySubCondJoin arg1) q
    | Object   [("BoolExpr",      Object      (("boolop", String "Or_EXPR")::("args",Array (arg1::q) )::_))] ->
                    L.fold_left ( fun a -> fun b -> Cond(Or,  a, trySubCondJoin b) ) (trySubCondJoin arg1) q

                    
    | json -> try CondExpre(parse_expreCondTerm json) with e ->  
                    failwith ("Unsupported condition join: " ^ validJsonOfJsont json)
    (*En gros on laisse parse_expreCondTerm se démerder| json -> failwith ("Unsupported condition join: " ^ validJsonOfJsont json)*)








           

(*- : t -> fromClause*)
and json_to_fromClause clause =
        (*TODO  f108 f110 f111 f113 m241 *)
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
        | Object [("JoinExpr", Object [("jointype", String jointype);
                                             ("larg", larg);
                                             ("rarg", rarg)
                                      ])] ->
                                                     let join_type = string_to_join_type jointype in
                                                     let larg = json_to_fromClause larg in
                                                     let rarg = json_to_fromClause rarg in
                                                     let quals = NA in
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
                                                     let rarg = try L.hd funcs |> parse_expreCondTerm with e -> prerr_endline "Failure HD json_to_fromClause"; ConstNull (*TODO bidouille*) in
                                                     JoinExpre (Lateral, larg, FromExpre rarg, NA)


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
                                                     JoinExpre (Lateral, larg, FromSubQuery rarg, NA)
                                                     

         | Object (("JoinExpr", Object (("jointype", String jointype)::("isNatural", Bool true)::
                                             ("larg", larg)::
                                             ("rarg", rarg)::_ ))::_) ->
                                                     let join_type = string_to_join_type jointype in
                                                     let larg = json_to_fromClause larg in
                                                     let rarg = json_to_fromClause rarg in
                                                     JoinExpre (Natural, larg, rarg, NA)

                                                     
        | Object [("RangeVar", Object (("schemaname",String schema)::("relname", String n)::("inh", Bool heritage)::("relpersistence", String persist)::("alias", Object [("aliasname", String alias)])::_))]
         -> FromExpre(TableChampRef(Some schema, n,alias))

        | Object [("RangeVar", Object (("relname", String n)::("inh", Bool heritage)::("relpersistence", String persist)::("alias", Object [("aliasname", String alias)])::_))]
         -> FromExpre(TableChampRef(None, n,alias))

        | Object [("RangeVar", Object  (("relname", String n)::("inh", Bool heritage)::("relpersistence", String persist)::_))]
        -> FromExpre(TableName(None,n))
        
        | Object [("RangeVar",     Object [
                ("schemaname", String schema);
                ("relname", String n);
                ("inh", Bool herit);
                ("relpersistence", String persist);
                ("location", _)]
           ) 
        ] -> FromExpre(TableName(Some schema, n))

        |  Object   (("RangeSubselect", Object (("subquery", subquery)::_))::_) -> FromSubQuery ( getOneStatement subquery  ) (*Il est normalement impossible qu'il y ait 2 subquery*)

        | Array (t::[]) -> json_to_fromClause t

        | Array l -> array_to_JoinExpreCross l



        | json -> let jsonprint = validJsonOfJsont json in
                let _ = prerr_endline jsonprint in
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
                let _ = prerr_endline jsonprint in
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
                | Array (  Object ( ("stmt", Object ( ("SelectStmt", Object(clauses) )::_) )::_ )::_ ) -> SelectStatement (L.map  getGoodClause clauses)
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
                | Object (("stmt", Object (("InsertStmt",Object l )::_))::_)
                        when (*L.mem_assoc "relname" l &&*) L.mem_assoc "selectStmt" l ->
                                let select = L.assoc  "selectStmt" l in
                                L.iter (fun (n,s) -> Printf.eprintf "%s=%s\n%!" n (validJsonOfJsont s) ) l;
                                getOneStatement select
                                (*match select with
                                | Object (("selectStmt", Object selectOK )::_) -> 
                                (*TODO TODO : faire un UpdateSelect et mettre le nom*)
                                        SelectStatement (L.map  getGoodClause selectOK)
                                | js -> failwith ("Unsupported getOneSelectQuery.2: " ^ validJsonOfJsont js)
                                *)
                | json -> failwith ("Unsupported getOneStatement: " ^ validJsonOfJsont json)

and  json2Grammar ( json : Tiny_json.Json.t) :  sqlEntry list =
        let printJsonList  = L.iter (fun elem -> validJsonOfJsont elem |> prerr_endline) in
                match json with
        | Object( version::("stmts",Array( stmts ) )::_   ) -> (*printJsonList stmt ;*)
                        let parsedList = L.map getOneStatement stmts in
                        parsedList
        | Object( version::("stmts",Object(_) )::_   ) -> (*printJsonList stmt ;*)
                        [getOneStatement json]
        | Object (("",  Object (("",  Object (("version", Number _)::("stmts",    stmts  )::_))::_   ))::_) ->
                        [getOneStatement stmts]
        | Object (("",  Object (("version", Number _)::("stmts",    stmts  )::_))::_   ) ->
                        [getOneStatement stmts]

        | _ -> failwith "pas pas  match"
        

;;



