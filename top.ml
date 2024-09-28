#require "tiny_json,jsonm,batteries";;
#load "JsonSqlParse.cmo";;
#load "sqlAnalyse.cmo";;
#load "jsonBadgerParse.cmo";;
open Tiny_json.Json;;
open JsonSqlParse;;
open JsonBadgerParse;;
open SqlAnalyse;;
let validJsonOfJsont  j =
        let buffer = Buffer.create 65535 in
        let formatter = Format.formatter_of_buffer buffer in
        Tiny_json.Json.format formatter j;
        Format.pp_print_flush formatter ();
        Buffer.contents buffer;;

#print_length 100000;;


#print_depth  500;;





#trace json_to_expreCond;;
#trace json_to_condJoin;;
#trace json_to_fromClause;;
(*#trace parseWithClause;;
#trace getOneStatement;;*)
#trace parse_whereClause;;
#trace parse_expreCondTerm;;

let file2string filename =
  let ic = open_in filename in
  let buffer = Buffer.create 1024 in
  try
    while true do
      let linha = input_line ic in
      Buffer.add_string buffer linha;
      Buffer.add_char buffer '\n'
    done;
    close_in ic;
    Buffer.contents buffer
  with
  | End_of_file ->
    close_in ic;
    Buffer.contents buffer;;


(*let grosseReq = file2string "grosseReq.json" |> Tiny_json.Json.parse;;

        
let gram = 
        let s = json2Grammar grosseReq in
                match s with 
                | SelectStatement gram -> gram
                | _ -> failwith "extract select" ;;

let from = List.filter_map (fun e -> match e with Some(From(fr,json)) -> Some(json) | _ -> None) gram |> List.hd ;;

let fromClause = json_to_fromClause from;;*)

let parseJsonFile filepath =
        let reqbrut = file2string filepath |> Tiny_json.Json.parse in 
        let s = json2Grammar reqbrut in
        s;;
        (*let gram = match s with 
                | SelectStatement gram -> gram
                | IndexCreation gram -> gram
                | _ -> failwith "extract select | IndexCreation" in
        gram*);;


(*let selectReq  = parseJsonFile "grosseReq.json";;
let indexesReq = parseJsonFile "indexesAST.json";;
*)

let getW s = match s with
| Where (_,j) -> Some(j)
|  _ -> None;;


let afrom = 
       (JoinExpre (Inner,
         JoinExpre (Inner,
          JsonSqlParse.FromExpre (TableChampRef (None,"ir_model_access", "a")),
          JsonSqlParse.FromExpre (TableChampRef (None,"ir_model", "m")),
          Cond (Equal, CondExpre (ColumnRef (Some "m", "id")),
           CondExpre (ColumnRef (Some "a", "model_id")))),
         JsonSqlParse.FromExpre (TableChampRef (None,"res_groups_users_rel", "gu")),
         Cond (Equal, CondExpre (ColumnRef (Some "gu", "gid")),
          CondExpre (ColumnRef (Some "a", "group_id")))));;
open SqlAnalyse;;

#trace expreCondTerm_to_data;;
#trace make_hashs;;
#trace from_to_data;;
#trace condexpre_to_data;;

module Sequence = struct
  (* Un ref pour stocker la valeur courante de la séquence *)
  let current_value = ref 0

  (* Fonction pour obtenir la valeur suivante de la séquence *)
  let next () =
    let value = !current_value in
    incr current_value;
    value
  let set v =
        current_value := v 
end;;
(*let whereClause = match selectReq |> List.hd with  SelectStatement gram -> List.filter_map (fun e -> match e with Some(f) -> getW f | None -> getW (Top(Null)) )  gram;;
let wc = List.hd whereClause;;*)
