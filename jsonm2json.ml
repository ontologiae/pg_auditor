module H = BatHashtbl;;
module L = BatList;;
module O = BatOption;;
module A = BatArray;;
module S = BatString;;

open Jsonm

(*TODO : Bidouille temporaire car pas le temps
 On parse avec Jsonm et on transforme en Tiny_json pour ne pas avoir à changer tout le code
 TODO modifier JsonSqlParse pour passer à  cet AST là*)
open Tiny_json.Json;;


type json =
  | Null
  | Bool of bool
  | Float of float
  | String of string
  | List of json list
  | Obj of (string * json) list;;


let rec parse_jsonm d name =
  let rec parse_value () =
    match Jsonm.decode d with
    | `Lexeme (`Null) -> Null
    | `Lexeme (`Bool b) -> Bool b
    | `Lexeme (`Float f) -> Float f
    | `Lexeme (`String s) -> String s
    | `Lexeme (`Os) -> parse_object []
    | `Lexeme (`As) -> parse_array []
    | `Lexeme (`Ae) -> failwith "Unexpected end of array"
    | `Lexeme (`Oe) -> failwith "Unexpected end of object"
    | `Lexeme (`Name n) ->  let value = parse_value () in
                                 parse_object ((n, value) :: []) (* Printf.sprintf "Unexpected name :%s" n |> failwith*)
    | `End -> failwith "Unexpected end of input"
    | `Error e -> failwith (Format.asprintf "Decode error: %a" Jsonm.pp_error e)
    | `Await -> failwith "Unexpected `Await in decoder"

  and parse_object acc =
    match Jsonm.decode d with
    | `Lexeme (`Oe) -> Obj (List.rev acc)
    | `Lexeme (`Name n) -> 
        let value = parse_value () in
        parse_object ((n, value) :: acc)
    | _ -> failwith "Expected object name or end of object"

  and parse_array acc =
    match Jsonm.decode d with
    | `Lexeme (`Ae) -> List (List.rev acc)
    | _ -> 
        let value = parse_value () in
        parse_array (value :: acc)
  in
  Obj[ (name,parse_value ())];;




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




let rec jsonmjsonAst2Tiny jast =
       match jast with
         | Null     -> Tiny_json.Json.Null
         | Bool  b   -> Tiny_json.Json.Bool b
         | Float f  ->  Tiny_json.Json.Number ( string_of_float f)
         | String s ->  Tiny_json.Json.String s
         | List l   -> Tiny_json.Json.Array (L.map jsonmjsonAst2Tiny l)
         | Obj l -> Tiny_json.Json.Object (L.map (fun (s,obj) -> s, jsonmjsonAst2Tiny obj) l);;

let readfile_and_parse_json_file file_path =
  let ic = open_in file_path in
  (*let json_input = really_input_string ic (in_channel_length ic) in*)
  
  let d = Jsonm.decoder (`Channel ic) in
  let res = parse_jsonm d "" in
  close_in ic;
  res;;


let read_2_tinyjson file_path =
        try let ast = readfile_and_parse_json_file file_path in
        jsonmjsonAst2Tiny ast
        with e -> file2string file_path |> Tiny_json.Json.parse;;


let string_2_tinyjson json_input =
  let d = Jsonm.decoder (`String json_input) in
  try 
   let ast = parse_jsonm d "" in
    jsonmjsonAst2Tiny ast
  with e -> Tiny_json.Json.parse json_input;;

