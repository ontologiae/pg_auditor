#require "tiny_json";;
open Tiny_json.Json;;

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


let grosseReq = file2string "grosseReq.json" |> Tiny_json.Json.parse;;
