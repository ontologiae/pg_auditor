open JsonBadgerParse;;
open Pg_query;;
open JsonSqlParse;;


module H = BatHashtbl;;
module L = BatList;;
module O = BatOption;;
module A = BatArray;;
module S = BatString;;

(*
type sample = {
  remote: string option;
  db: string;
  plan: string option;
  bind: string option;
  query: string;
  date: string;
  user: string;
  app: string;
}

type query_info = {
  total_duration: float;
  samples: (float, sample) Hashtbl.t;
  count: int;
  apps: (string, user_app_info) Hashtbl.t;
  max: float;
  users: (string, user_app_info) Hashtbl.t;
  chronos: chronos_info;
  min: float;
  query:string;
}
*)


(*TODO : 
        * l'id de la query est le numéro de la requête dans la liste
        * Pour chaque sample, on incrémente un nombre    *)


module Sequence = struct
  (* Type de la séquence *)
  type t = { mutable value : int }

  (* Fonction pour créer une nouvelle séquence *)
  let create () = { value = 0 }

  let get seq = seq.value;;

  (* Fonction pour obtenir la valeur suivante de la séquence *)
  let next seq =
    let value = seq.value in
    seq.value <- seq.value + 1;
    value
end;;


(* En singleton
module Sequence = struct
  (* Un ref pour stocker la valeur courante de la séquence *)
  let current_value = ref 0

  (* Fonction pour obtenir la valeur suivante de la séquence *)
  let next () =
    let value = !current_value in
    incr current_value;
    value
end;;  *)

(*Global, car on génère une base*)
let sampleSeq = Sequence.create();;
let fromNodeSeq   = Sequence.create();; 
let querySeq  = Sequence.create();;
let fromGlobalSeq   = Sequence.create();; 


let sql_mr_propre str = 
        let s1 = S.nreplace ~str:str  ~sub:"'" ~by:"''"  in
        S.nreplace ~str:s1  ~sub:"\n" ~by:" "


(*int -> (float, JsonBadgerParse.query_info) Hashtbl.t -> string list = <fun>*)
let samplesToSql idx sampleInfo =
        let l = H.to_list sampleInfo in
        let sampleToSql time query = Printf.sprintf "Insert into samples(id,queryId,time,query) values (%d,%d,%f,'%s');\n" (Sequence.next sampleSeq) idx time (sql_mr_propre query) in (*TODO remplacer \n et ' par ''*) 
        L.map (fun (time,smpl) -> sampleToSql time smpl.query_wparam) l;;







let find_key_by_value hashtbl value =
  let found_key = ref None in
  BatHashtbl.iter (fun key v ->
    if v = value then found_key := Some key
  ) hashtbl;
  !found_key;;





let expreCond2String e =
        match e with
        | TableChampRef(_,__,_) -> "TableChampRef"
        | TableName _ -> "TableName"
        | FunctionCall(_,_) -> "FunctionCall"
        | ColumnRef(_,_) -> "ColumnRef"
        | ExpreSubQuery(_,_) -> "ExpreSubQuery"
        | ConstStr _ -> "ConstStr"
        | ConstNbr _ -> "ConstNbr"
        | ConstTimestamp _ -> "ConstTimestamp"
        | ConstDate _ -> "ConstDate"
        | ConstBool _ -> "ConstBool"
        | TypeCast(_,_) -> "TypeCast"
        | ListTerm _ -> "ListTerm"
        | ParamRef _ -> "ParamRef"
        | ArgStarAll -> "ArgStarAll"
        | ConstNull -> "ConstNull";;




(*
- : Sequence.t -> (string, int) Hashtbl.t -> (string, int) Hashtbl.t -> expreCondTerm 
        -> int * string * string option
*)
let rec expreCondTerm_to_data  seq table_ids alias_ids expre : (int * string * string option) = (*L'id de la table, le nom de la table et le nom de la colonne*) 
        printHashDebug alias_ids;printHashDebug table_ids;
        match expre with
        | ColumnRef (Some alias, colonne) ->
                        begin match Hashtbl.find_opt alias_ids alias with
                          | Some id ->
                                          let table_name = find_key_by_value table_ids id in
                                          (id, table_name |> O.get (*TODO : bouhhh !!*), Some colonne)
                          | None -> failwith "expreCondTerm_to_data, cas ColumnRef (Some alias, colonne) et rien dans la Hashtbl : alias Inconnu"
                        end
        | TableChampRef (schema,table_name, table_alias) ->
                          begin match Hashtbl.find_opt alias_ids table_alias with
                          | Some id ->
                              (id, table_name,None )
                          | None ->
                              let id = Sequence.next seq in
                              Hashtbl.add table_ids table_name id;
                              Hashtbl.add alias_ids table_alias id;
                              (id, table_name, None)
                          end
         | TableName (_,table_name) ->
                          begin 
                              let id = Sequence.next seq in
                              Hashtbl.add table_ids table_name id;
                              (id, table_name, None)
                          end

         | autre -> expreCond2String autre |> failwith
(*- : Sequence.t ->
    (string, int) Hashtbl.t ->
    (string, int) Hashtbl.t -> condJoin -> (int * string * string option) list *)
 and condexpre_to_data seq table_ids alias_ids cond_clause : (int * string * string option ) list = (*L'id de la table, le nom de la table, le nom de la colonne *)
        match cond_clause with
        | Cond (op, left, right) ->
                        let res = (condexpre_to_data seq table_ids alias_ids left )@(condexpre_to_data seq table_ids alias_ids right ) in
                        res
        | CondExpre(expre) -> [expreCondTerm_to_data seq table_ids alias_ids expre] 

(*
- : SqlAnalyse.Sequence.t -> (string, int) Hashtbl.t -> (string, int) Hashtbl.t ->
    fromClause ->
    int ->
    joinType option ->
    string option ->
    string option ->
    (int * string * int option * joinType option * string option * string option) list
*)
and printHashDebug h = H.iter (fun k -> fun v -> Printf.eprintf "K=%s, V=%d\n%!" k v) h 
and make_hashs seq table_ids alias_ids from_clause =
        match from_clause with
        | JoinExpre (join_type, left_clause, right_clause, cond) ->     make_hashs seq table_ids alias_ids left_clause; 
                                                                        make_hashs seq table_ids alias_ids right_clause
        | FromExpre expre -> ignore(expreCondTerm_to_data seq  table_ids alias_ids expre) (*On lui donne une seq dans le vent pour éviter les effets de bords, et on rempli la hash*)
        | FromSubQuery _ -> () (*TODO : triater le cas des sous requêtes*)

and fromClause_to_data seq table_ids alias_ids from_clause parent_id join_op column_opt
                                                (*parent_column_opt : string option*) 
                                                (res :  (int * string * int option * joinType option * string option ) list)
                                        : (int * string * int option * joinType option * string option ) list =
                                                (* id *  tablename * id du from père * op * champ de relation *)
        printHashDebug alias_ids;printHashDebug table_ids;
  match from_clause with

  (*
  JoinExpre (Inner, FromExpre (TableChampRef ("ir_model_access", "a")),
   FromExpre (TableChampRef ("ir_model", "m")),
   Cond (Equal, CondExpre (ColumnRef (Some "m", "id")),
    CondExpre (ColumnRef (Some "a", "model_id"))))

        Exemple de règle  : le parentid du membre de droite pointe sur l'id du membre de gauche en terme SI le membre de droite se réfère au membre de gauche
        Dans cet algo, il faut faire 2 passes : une passe de dépendance et une passe de liens...

        L'algo est beaucoup trop basique, on doit construire le graph, puis le représenter
        2ième problème : il faut une table des tables pour remplacer les tables par des ids
        Le système actuel devrait fonctionner dans la plupart des cas, modulo les sous requêtes...
   
   *)
  | JoinExpre (join_type, left_clause, right_clause, cond) ->
                make_hashs seq table_ids alias_ids from_clause;
                let conds =  condexpre_to_data seq table_ids alias_ids cond  in
                let id_left, table_left, column_left = try L.hd conds with e -> prerr_endline "condexpre_to_data rien dans le membre de gauche !";(-1,"unknown",None) in
                let id_right, table_right, column_right = try L.at conds 1 with e -> prerr_endline "condexpre_to_data rien dans le membre de gauche !";(-1,"unknown",None) in
                let parentId tblname curid =
                        let id = Hashtbl.find_opt  table_ids tblname in
                        match id, curid with
                        | Some (id), cur when id = cur -> Printf.eprintf "tblname=%s id=%d cur=%d\n%!" tblname id cur; None (*C'est sans doute la table qui se référence elle-même, donc on renvoi None pour Null dans la table*)
                        | Some (id), cur -> Some(id) (*Lien vers table père pour la jointure*)
                        | _,_ -> failwith "fromClause_to_data JoinExpre parentId | _,_ " in
                let res_final = (id_left, table_left, parentId table_left id_left, Some join_type, column_left)
                                        ::(id_right, table_right, Some(id_left), Some join_type, column_right)
                                        ::res in
                let left_data = from_to_data seq table_ids alias_ids left_clause parent_id (Some join_type) (column_left)  res_final in
                let right_data = from_to_data seq table_ids alias_ids right_clause id_left (Some join_type) (column_right) res_final in
                left_data @ right_data
  | FromExpre (TableName (_,tn)) -> [(Sequence.next seq, tn, None, None, None)]
  | FromExpre expre->
                  let id, table_name, colonne = expreCondTerm_to_data seq table_ids alias_ids expre in
                  begin
                          match colonne with
                          | Some colname ->
                                  (id, table_name, (if parent_id = id then None else Some(parent_id)), join_op, colonne)::res
                          | None -> printHashDebug table_ids; printHashDebug alias_ids; Printf.eprintf "Colonne id=%d\n%!" id; res
                                          (*failwith "from_to_data cas FromExpre expre match colonne None : Pas de nom de colonne"*)
                  end
(*FromSubQuery TODO*)
  | _ -> []
and from_to_data seq table_ids alias_ids from_clause parent_id join_op column_opt res =
        fromClause_to_data seq table_ids alias_ids from_clause parent_id join_op column_opt res |> L.unique;;



let getFrom queryAST =
        let getFrom sqlEntry = match sqlEntry with (*TODO, il faut chercher les from DANS les sous requêtes et les With !*)
                                | SelectStatement(parts)  -> L.filter_map (fun el -> match el with | Some(From(res,_)) -> Some(res) | _ -> None )  parts |> L.hd
                                | _ -> failwith "Aucune clause From dans la requête" (* *) in
        getFrom queryAST
        ;;




let query_infoToSql  queryinfo ast =
        Printf.eprintf "Analyse de : %s\n%!" queryinfo.query;
        let joinType_to_string jt = match jt with
        | Inner -> "Inner" 
        | FullOuter -> "FullOuter" 
        | Left -> "Left" 
        | Right -> "Right" 
        | Lateral -> "Lateral" 
        | Cross -> "Cross" 
        | Natural -> "Natural" in
        let cur_query_id = (Sequence.next querySeq) in
        let req_query = Printf.sprintf "Insert into Query(id,req,totaltime,max,min) values(%d,'%s',%f::real,%f::real,%f::real);\n"
                 cur_query_id queryinfo.query  queryinfo.total_duration queryinfo.max queryinfo.min in
        let reqsSamples = samplesToSql cur_query_id queryinfo.samples in
        let fromAst = getFrom ast in
        (*let reqsFromLst = try from_to_data fromNodeSeq (H.create 1) (H.create 1) fromAst (Sequence.next fromNodeSeq) None None [] with e -> [] in*)
        let reqsFromLst = from_to_data fromNodeSeq (H.create 1) (H.create 1) fromAst (Sequence.next fromNodeSeq) None None [] in 
        Printf.eprintf "On a %d éléments From\n%!" (L.length reqsFromLst);
        let reqsFromStrLst = L.map (fun (id, table, parentId, join_type, column) ->
                                let goodpid = match parentId with
                                | Some id -> Printf.sprintf "%d" id
                                | None -> "NULL" in
                                let jtyp = O.default Cross join_type in
                                let col  = O.default "NULL" column in
                                Printf.sprintf "Insert into Froms(id,nodeid,queryid, tablename, idfrom, joinType, columname) values (%d,%d,%d, '%s', %s, '%s', '%s');\n" 
                                                                 (Sequence.next fromGlobalSeq) id cur_query_id table goodpid (joinType_to_string jtyp) col) reqsFromLst in
        print_endline req_query;
        S.join "" reqsSamples |> print_endline;
        S.join "" reqsFromStrLst |> print_endline;

;;
