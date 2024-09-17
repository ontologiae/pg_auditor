#require "jsonm";;
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
type user_app_info = {
  duration: float;
  count: int;
}
type min_duration_info = {
  duration: float;
}
type min_info = {
  count: int;
}
type chronos_hour_info = {
  count: int;
  duration: float;
  min_duration: (float, min_duration_info) Hashtbl.t;
  min: (float, min_info) Hashtbl.t ;
}
type chronos_day_info = {
  hours: (int, chronos_hour_info) Hashtbl.t;
}
type chronos_info = {
  day_info: (int, chronos_day_info) Hashtbl.t;
}
type query_info = {
  duration: float;
  samples: (float, sample) Hashtbl.t;
  count: int;
  apps: (string, user_app_info) Hashtbl.t;
  max: float;
  users: (string, user_app_info) Hashtbl.t;
  chronos: chronos_info;
  min: float;
}
type root = {
  queryProto : query_info list;
}
(*
1. **`sample`** : Représente chaque échantillon de la section `samples`.
2. **user_app_info`** : Représente les informations des applications dans la section `apps`.
4. **`min_duration_info`** et **`min_info`** : Représentent les informations dans la section `min_duration` et `min` respectivement.
5. **`chronos_hour_info`** : Représente les informations pour une heure spécifique dans `chronos`.
6. **`chronos_day_info`** : Représente les informations pour un jour spécifique dans `chronos`.
7. **`chronos_info`** : Regroupe toutes les informations de `chronos`.
8. **`query_info`** : Représente les informations de la requête `SELECT From Where`.
9. **`postgres_info`** : Regroupe les informations spécifiques à PostgreSQL.
10. **`normalized_info`** : Regroupe les informations normalisées.
11. **`root`** : Représente la racine de l'arborescence JSON.*)
open Jsonm


(* Helper function to decode JSON key-value pairs of various types. *)
let expect_string d =
  match Jsonm.decode d with
  | `Lexeme (`String s) -> s
  | _ -> failwith "Expected string";;
let expect_float d =
  match Jsonm.decode d with
  | `Lexeme (`Float f) -> f
  | _ -> failwith "Expected float";;
let expect_int d =
  match Jsonm.decode d with
  | `Lexeme (`Float f) -> int_of_float f
  | _ -> failwith "Expected int";;
let expect_name d =
  match Jsonm.decode d with
  | `Lexeme (`Name n) -> n
  | _ -> failwith "Expected name";;


(*
- : ((string *
      ([> `A of 'b list
        | `Bool of bool
        | `Float of float
        | `Null
        | `O of 'a list
        | `String of string ]
       as 'b)
      as 'a)
     list -> 'a list) ->
    'a list ->
    decoder ->
    [> `Await | `End | `Error of error | `Lexeme of lexeme ] -> 'a list
= <fun>
 
 *)
let rec subdecode_object aux acc d o = 
        let _ = match o with `Lexeme l ->   Format.printf "subdecode_object DEBUG init = %a %!\n" Jsonm.pp_lexeme l | _ -> () in
  match o with
  | `Lexeme `Oe -> List.rev acc
  | `Lexeme (`Name name) ->
    let value = decode_value d in
    aux ((name, value) :: acc)
  | `Lexeme `Os -> (* Cas d'objet imbriqué *)
    let subobj = decode_object d in
    let remaining = aux acc in
    subdecode_object aux (("nested_object", `O subobj) :: remaining) d o
  | `Lexeme `As -> (* Cas de tableau imbriqué *)
    let subarray = decode_array d in
    let remaining = aux acc in
    subdecode_object aux (("nested_array", `A subarray) :: remaining) d o
  | `Lexeme l -> failwith (Format.asprintf "subdecode_object : Unexpected lexeme: %a" Jsonm.pp_lexeme l)
  | `Error e -> failwith (Format.asprintf "subdecode_object : Decode error: %a" Jsonm.pp_error e)
  | _ -> failwith "subdecode_object : Expected object field"
(* Decode JSON object into (string * 'a) list *)
and decode_object d =
        let decoded = Jsonm.decode d in
        let rec aux acc =  subdecode_object aux acc d decoded in
       match decoded with
        | `End  -> failwith "decode_object : FIN !"
        | `Lexeme l -> Format.printf "decode_object DEBUG = %a %!" Jsonm.pp_lexeme l;aux []
        | _     ->  aux []
(* Decode JSON array into 'a list *)
and decode_array d =
  let rec aux acc =
    match Jsonm.decode d with
    | `Lexeme `Ae -> List.rev acc
    | _ ->
      let value = decode_value d in
      aux (value :: acc)
  in
  aux []
(* Decode any JSON value *)
and decode_value d =
  let tok = Jsonm.decode d in
  match tok with
  | `Lexeme `Null -> `Null
  | `Lexeme (`Bool b) -> `Bool b
  | `Lexeme (`String s) -> `String s
  | `Lexeme (`Float f) -> `Float f
  (*| `Lexeme (`Name name) -> `O (decode_object d)*)
  | `Lexeme `Os -> `O (decode_object d)
  | `Lexeme `As -> `A (decode_array d)
  | `Lexeme l -> failwith (Format.asprintf "decode_value : Unexpected lexeme: %a" Jsonm.pp_lexeme l)
  | `Error e -> failwith (Format.asprintf "decode_value : Decode error: %a" Jsonm.pp_error e)
  | _ -> failwith "decode_value : C'est autre chose";;
 (* | _ -> failwith "decode_value : C'est autre chose";;*)
#trace decode_object;;
#trace decode_value;;

(* Decode "sample" object *)
let decode_sample d =
  let obj = decode_object d in
  let get_opt key =
    try Some (List.assoc key obj |> function `String s -> s | _ -> failwith "Expected string")
    with Not_found -> None
  in
  let get_str key = List.assoc key obj |> function `String s -> s | _ -> failwith "Expected string" in {
    remote = get_opt "remote";
    db = get_str "db";
    plan = get_opt "plan";
    bind = get_opt "bind";
    query = get_str "query";
    date = get_str "date";
    user = get_str "user";
    app = get_str "app";
  };;
(* Decode "app_info" object *)
let decode_app_info d =
  let obj = decode_object d in
  let get_int key = List.assoc key obj |> function `Float f -> int_of_float f | _ -> failwith "Expected int" in
  let get_float key = List.assoc key obj |> function `Float f -> f | _ -> failwith "Expected float" in {
    count = get_int "count";
    duration = get_float "duration";
  };;
(* Decode "user_info" object *)
let decode_user_info d =
  let obj = decode_object d in
  let get_int key = List.assoc key obj |> function `Float f -> int_of_float f | _ -> failwith "Expected int" in
  let get_float key = List.assoc key obj |> function `Float f -> f | _ -> failwith "Expected float" in {
    duration = get_float "duration";
    count = get_int "count";
  };;
(* Decode "chronos_hour_info" object *)
let decode_chronos_hour_info d =
  let obj = decode_object d in
  let get_int key = List.assoc key obj |> function `Float f -> int_of_float f | _ -> failwith "Expected int" in
  let get_float key = List.assoc key obj |> function `Float f -> f | _ -> failwith "Expected float" in
  let min_duration_tbl = Hashtbl.create 10 in
  let min_tbl = Hashtbl.create 10 in
  (match List.assoc_opt "min_duration" obj with
  | Some (`O min_duration_obj) ->
    List.iter (fun (k, v) -> Hashtbl.add min_duration_tbl (float_of_string k) { duration = (match v with `Float f -> f | _ -> failwith "Expected float") }) min_duration_obj
  | _ -> ());
  (match List.assoc_opt "min" obj with
  | Some (`O min_obj) ->
    List.iter (fun (k, v) -> Hashtbl.add min_tbl (float_of_string k) { count = (match v with `Float f -> int_of_float f | _ -> failwith "Expected float") }) min_obj
  | _ -> ()); {
    count = get_int "count";
    duration = get_float "duration";
    min_duration = min_duration_tbl;
    min = min_tbl;
  };;
(* Decode "chronos_info" object *)
let decode_chronos_info d =
  let obj = decode_object d in
  let days_tbl = Hashtbl.create 10 in
  (match List.assoc_opt "20240606" obj with
  | Some (`O hours_obj) ->
    List.iter (fun (hour, hour_info) ->
        Hashtbl.add days_tbl (int_of_string hour) {
          hours = let hours_tbl = Hashtbl.create 10 in 
          (match hour_info with 
          | `O hour_info_list -> 
            List.iter (fun (k, v) -> 
              Hashtbl.add hours_tbl (int_of_string k) (decode_chronos_hour_info d)
            ) hour_info_list;
            hours_tbl
          | _ -> failwith "decode_chronos_info : Incorrect json format"
          ) 
      }) hours_obj
  | _ -> ()); {
    day_info = days_tbl;
  };;
(* Decode "query_info" object *)
let decode_query_info d =
  let obj = decode_object d in
  let get_float key =  List.assoc key obj |> function `Float f -> f | _ -> failwith "Expected float" in
  let get_int key = List.assoc key obj |> function `Float f -> int_of_float f | _ -> failwith "Expected int" in
  {
    duration = get_float "duration";
    samples = (match List.assoc "samples" obj with
      | `O tbl_obj -> 
        let samples_tbl = Hashtbl.create 10 in
        List.iter (fun (k, v) -> Hashtbl.add samples_tbl (float_of_string k) (decode_sample d)) tbl_obj;
        samples_tbl
      | _ -> failwith "decode_query_info : Expected samples object");
    count = get_int "count";
    apps = (match List.assoc "apps" obj with
      | `O tbl_obj -> 
        let apps_tbl = Hashtbl.create 10 in
        List.iter (fun (k, v) -> Hashtbl.add apps_tbl k (decode_app_info d)) tbl_obj;
        apps_tbl
      | _ -> failwith "decode_query_info : Expected apps object");
    max = get_float "max";
    users = (match List.assoc "users" obj with
      | `O tbl_obj -> 
        let users_tbl = Hashtbl.create 10 in
        List.iter (fun (k, v) -> Hashtbl.add users_tbl k (decode_user_info d)) tbl_obj;
        users_tbl
      | _ -> failwith "decode_query_info : Expected users object");
    chronos = decode_chronos_info d;
    min = get_float "min";
  };;
(* Decode the root object *)
let decode_root d =
  let obj = decode_object d in
  {
    queryProto = (match List.assoc "normalyzed_info" obj with
      | `O queries -> List.map (fun (_, v) -> decode_query_info d) queries
      | _ -> failwith "decode_root : Expected normalyzed_info object");
  };;
let rec print_lexemes d =
  match Jsonm.decode d with
  | `Lexeme l -> 
      begin
        match l with
        | `Null -> Printf.printf "Lexeme: Null\n"
        | `Bool b -> Printf.printf "Lexeme: Bool - %b\n" b
        | `String s -> Printf.printf "Lexeme: String - %s\n" s
        | `Float f -> Printf.printf "Lexeme: Float - %f\n" f
        | `Name n -> Printf.printf "Lexeme: Name - %s\n" n
        | `Os -> Printf.printf "Lexeme: Start of Object\n"
        | `Oe -> Printf.printf "Lexeme: End of Object\n"
        | `As -> Printf.printf "Lexeme: Start of Array\n"
        | `Ae -> Printf.printf "Lexeme: End of Array\n"
      end;
      print_lexemes d
  | `End -> Printf.printf "Lexeme: End of JSON\n"
  | `Error e -> failwith (Format.asprintf "decode_value : Decode error: %a" Jsonm.pp_error e)
  | `Await -> failwith "Unexpected `Await in decoder";;

let print_decoded_json_string json_str =
  let d = Jsonm.decoder (`String json_str) in
  print_lexemes d;;
let expl = "{
\"normalyzed_info\" : {
      \"postgres\" : {
\"SELECT From Where\" : {
            \"duration\" : 0.826,
            \"samples\" : {
               \"0.167\" : {
                  \"remote\" : null,
                  \"db\" : \"adep\",
                  \"plan\" : null,
                  \"bind\" : null,
                  \"query\" : \"SELECT From Where\",
                  \"date\" : \"2024-06-06 23:09:18\",
                  \"user\" : \"openassur\",
                  \"app\" : \"[unknown]\"
               },
               \"0.184\" : {
                  \"plan\" : null,
                  \"db\" : \"adep\",
                  \"bind\" : null,
                  \"remote\" : null,
                  \"app\" : \"[unknown]\",
                  \"user\" : \"openassur\",
                  \"date\" : \"2024-06-06 23:08:16\",
                  \"query\" : \"SELECT From Where\"
               },
               \"0.312\" : {
                  \"remote\" : null,
                  \"plan\" : null,
                  \"db\" : \"adep\",
                  \"bind\" : null,
                  \"date\" : \"2024-06-06 23:09:18\",
                  \"query\" : \"SELECT From Where\",
                  \"app\" : \"[unknown]\",
                  \"user\" : \"myuser\"
               }
            },
            \"count\" : 4,
            \"apps\" : {
               \"[unknown]\" : {
                  \"count\" : 4,
                  \"duration\" : 0.826
               }
            },
            \"max\" : \"0.312\",
            \"users\" : {
               \"myuser\" : {
                  \"duration\" : 0.826,
                  \"count\" : 4
               }
            },
            \"chronos\" : {
               \"20240606\" : {
                  \"23\" : {
                     \"count\" : 4,
                     \"duration\" : 0.826,
                     \"min_duration\" : {
                        \"08\" : 0.347,
                        \"09\" : 0.479
                     },
                     \"min\" : {
                        \"09\" : 2,
                        \"08\" : 2
                     }
                  }
               }
            },
            \"min\" : \"0.163\"
         }
}
 }
}";;

let decode_json_string json_str =
  let d = Jsonm.decoder (`String json_str) in
  match decode_root d with
  | exception e -> Printf.printf "Error while decoding: %s\n" (Printexc.to_string e)
  | root -> 
    (* Here you can work with your decoded root object *)
    Printf.printf "Decoded root object: %d\n" (List.length root.queryProto)
;;
(*
Normaliz -> Postgres -> Querysubj -> Sample -> Duration;
Duration -> Sample;
Sample -> Querysubj;
Querysubj -> Apps -> Appname -> Apps  -> Querysubj;
Querysubj -> Users -> User   -> Users -> Querysubj;
User -> Users -> Querysubj;
Querysubj -> Chrono -> Day -> Heure -> Min_duration -> Heure;
Heure -> Min -> Heure -> Day -> Chrono -> Querysubj;
Chrono -> Min -> Querysubj;
Querysubj -> Postgres -> Normaliz;*)

type state =
  | Normaliz
  | Postgres
  | Querysubj
  | Sample
  | Duration
  | Apps
  | Appname
  | Users
  | User
  | Chrono
  | Day
  | Heure
  | Min
  | Min_duration;;

let transition state =
  match state with
  | Normaliz -> Postgres
  | Postgres -> Querysubj
  | Querysubj -> Sample
  | Sample -> Duration
  | Duration -> Sample
  | Sample -> Querysubj
  | Querysubj -> Apps
  | Apps -> Appname
  | Appname -> Apps
  | Apps -> Querysubj
  | Users -> User
  | User -> Users
  | Users -> Querysubj
  | Querysubj -> Users
  | Querysubj -> Chrono
  | Chrono -> Day
  | Day -> Heure
  | Heure -> Min
  | Min -> Heure
  | Heure -> Day
  | Day -> Chrono
  | Chrono -> Querysubj
  | Querysubj -> Postgres
  | Postgres -> Normaliz
  | _ -> failwith "Invalid transition";;




 Querysubj, "samples" -> Sample
 Sample, is_float -> Duration
 Querysubj, "apps" -> Apps
 Apps , ? -> Appname
 Querysubj, "users" -> Users
 Users, ? -> User
 Querysubj, "chronos" -> Chrono
 Chrono, is_int -> Day
 Day, is_int  -> Heure
 Heure, "min_duration" -> Min_duration
 Heure, "min" -> Min 

let is_float s = 
  try ignore (float_of_string s); true
  with Failure _ -> false

let is_int s = 
  try ignore (int_of_string s); true
  with Failure _ -> false

let transition state input =
  match state, input with
  | Normaliz, _ -> Postgres
  | Postgres, _ -> Querysubj
  | Querysubj, "samples" -> Sample
  | Sample, s when is_float s -> Duration
  | Duration, _ -> Sample
  | Sample, _ -> Querysubj
  | Querysubj, "apps" -> Apps
  | Apps, _ -> Appname
  | Appname, _ -> Apps
  | Apps, _ -> Querysubj
  | Querysubj, "users" -> Users
  | Users, _ -> User
  | User, _ -> Users
  | Users, _ -> Querysubj
  | Querysubj, "chronos" -> Chrono
  | Chrono, s when is_int s -> Day
  | Day, s when is_int s -> Heure
  | Heure, "min_duration" -> Min_duration
  | Heure, "min" -> Min
  | Min_duration, _ -> Heure
  | Min, _ -> Heure
  | Heure, _ -> Day
  | Day, _ -> Chrono
  | Chrono, "min" -> Min
  | Chrono, _ -> Querysubj
  | Querysubj, _ -> Postgres
  | Postgres, _ -> Normaliz
  | _ -> failwith "Invalid transition"

let rec execute path state =
  match path with
  | [] -> state
  | input :: rest -> 
      let next_state = transition state input in
      execute rest next_state

  let tok = Jsonm.decode d in
  match tok with
  | `Lexeme `Null -> `Null
  | `Lexeme (`Bool b) -> `Bool b
  | `Lexeme (`String s) -> `String s
  | `Lexeme (`Float f) -> `Float f
  (*| `Lexeme (`Name name) -> `O (decode_object d)*)
  | `Lexeme `Os -> `O (decode_object d)
  | `Lexeme `As -> `A (decode_array d)
  | `Lexeme l -> failwith (Format.asprintf "decode_value : Unexpected lexeme: %a" Jsonm.pp_lexeme l)
  | `Error e -> failwith (Format.asprintf "decode_value : Decode error: %a" Jsonm.pp_error e)
  | _ -> failwith "decode_value : C'est autre chose";;

