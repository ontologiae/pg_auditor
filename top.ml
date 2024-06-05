#require "tiny_json";;
#load "JsonSqlParse.cmo";;
open Tiny_json.Json;;
open JsonSqlParse;;

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


let selectReq  = parseJsonFile "grosseReq.json";;
let indexesReq = parseJsonFile "indexesAST.json";;
