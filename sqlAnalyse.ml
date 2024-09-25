open JsonBadgerParse;;
open Pg_query;;
open JsonSqlParse;;


module H = BatHashtbl;;
module L = BatList;;
module O = BatOption;;
module A = BatArray;;


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


let sampleSeq = Sequence.create();;
let fromSeq   = Sequence.create();; 


let samplesToSql idx sampleInfo =
        let l = H.to_list sampleInfo in
        let sampleToSql time query = Printf.sprintf "Insert into samples(id,queryId,time,query) values (%d,%d,%f,%s);\n" (Sequence.next sampleSeq) idx time query in
        L.map (fun (time,smpl) -> sampleToSql time smpl.query) l;;



        | JoinExpre of joinType * fromClause * fromClause * condJoin
        | CondExpre of expreCondTerm
        | FromSubQuery of sqlEntry
        | Inconnu (*Grammaire non gérée*)







let find_key_by_value hashtbl value =
  let found_key = ref None in
  BatHashtbl.iter (fun key v ->
    if v = value then found_key := Some key
  ) hashtbl;
  !found_key;;


let rec to_data seq table_ids alias_ids from_clause parent_id join_op column_opt parent_column_opt : (int * string * int option * joinType option * string option * string option) list =
  match from_clause with
  | JoinExpre (join_type, left_clause, right_clause, Cond (op, CondExpre (ColumnRef (Some alias_left, column_left)), CondExpre (ColumnRef (Some alias_right, column_right)))) ->
      let left_data = to_data seq table_ids alias_ids left_clause parent_id (Some join_type) (Some column_left) parent_column_opt in
      let right_data = to_data seq table_ids alias_ids right_clause parent_id (Some join_type) (Some column_right) (Some column_left) in
      left_data @ right_data
  | CondExpre (TableChampRef (table_name, table_alias)) ->
      begin match Hashtbl.find_opt alias_ids table_alias with
      | Some id ->
          [(id, table_name, (if parent_id = id then None else Some(parent_id)), join_op, column_opt, parent_column_opt)]
      | None ->
          let id = Sequence.next seq in
          Hashtbl.add table_ids table_name id;
          Hashtbl.add alias_ids table_alias id;
          [(id, table_name, Some(parent_id), join_op, column_opt, parent_column_opt)]
      end
      (*TODO :Duplication de code !!*)
  | CondExpre (ColumnRef ( Some table_alias, columnName)) ->
      begin match Hashtbl.find_opt alias_ids table_alias with
      | Some id -> (*l'ID suffit pas : il faut le nom de la table à partir de l'ID pour la mettre dans la ligne TODO : SALE !*)
          [(id, find_key_by_value table_ids id |> O.get, 
                                (if parent_id = id then None else Some(parent_id)), join_op, column_opt, parent_column_opt)]
      | None ->
          let id = Sequence.next seq in
          Hashtbl.add alias_ids table_alias id;
          [(id, "?", (if parent_id = id then None else Some(parent_id)), join_op, column_opt, parent_column_opt)]
      end
  | _ -> [];;

let getFromsAnalysis queryAST =
        let getFrom sqlEntry = match sqlEntry with (*TODO, il faut chercher les from DANS les sous requêtes et les With !*)
                                | SelectStatement(parts)  -> L.filter_map (fun el -> match Some(From(_,_)) -> el | _ -> None )  parts |> L.hd
                                | _ -> failwith "Aucune clause From dans la requête" (* *) in
        let from = getFrom queryAST in
        let getJoins
        



let query_infoToSql idx queryinfo =
        "Insert into...";;
