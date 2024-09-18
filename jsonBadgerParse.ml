#require "jsonm, batteries";;

module H = Hashtbl;;

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

(*
Normaliz -> Postgres -> Querysubj -> Samples -> Sample;
Sample -> Samples;
Samples -> Querysubj;
Querysubj -> Apps -> Appname -> Apps  -> Querysubj;
Querysubj -> Users -> User   -> Users -> Querysubj;
User -> Users -> Querysubj;
Querysubj -> Chrono -> Day -> Heure -> Min_duration -> Heure;
Heure -> Min -> Heure -> Day -> Chrono -> Querysubj;
Chrono -> Min -> Querysubj;
Querysubj -> Postgres -> Normaliz;*)

type state =
  | Root
  | Normaliz
  | Postgres
  | Querysubj
  | Sampless
  | Samples
  | Sample
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
  | Root -> Normaliz
  | Normaliz -> Postgres
  | Postgres -> Querysubj
  | Querysubj -> Samples
  | Samples -> Sample
  | Sample -> Samples
  | Samples -> Querysubj
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



 (*Querysubj, "samples" -> Samples
 Samples, is_float -> Sample
 Querysubj, "apps" -> Apps
 Apps , ? -> Appname
 Querysubj, "users" -> Users
 Users, ? -> User
 Querysubj, "chronos" -> Chrono
 Chrono, is_int -> Day
 Day, is_int  -> Heure
 Heure, "min_duration" -> Min_duration
 Heure, "min" -> Min *)



let getRoot str = Jsonm.decoder (`String str);;

let subjson = "{
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
                  }";;

let printState new_state = 
        match new_state with
         | Root -> "Root"
         | Normaliz -> "Normaliz"
         | Postgres -> "Postgres"
         | Querysubj -> "Querysubj"
         | Samples -> "Samples"
         | Sample -> "Sample"
         | Apps -> "Apps"
         | Appname -> "Appname"
         | Users -> "Users"
         | User -> "User"
         | Chrono -> "Chrono"
         | Day -> "Day"
         | Heure -> "Heure"
         | Min_duration -> "Min_duration"
         | Min -> "Min";;



let previous_state current_state =
  match current_state with
  | Postgres -> Normaliz
  | Querysubj -> Postgres
  | Samples -> Querysubj
  | Sample -> Samples
  | Appname -> Apps
  | Apps -> Querysubj
  | User -> Users
  | Users -> Querysubj
  | Day -> Chrono
  | Heure -> Day
  | Min_duration -> Heure
  | Min -> Chrono
  | Chrono -> Querysubj
  | Normaliz -> Root
  | Root -> Root
  | other -> printState other |> failwith;;


let is_float s = 
  try ignore (float_of_string s); true
  with Failure _ -> false

let is_int s = 
  try ignore (int_of_string s); true
  with Failure _ -> false

let transition state input =
  match state, input with
  | Root, "normalyzed_info" -> Normaliz
  | Normaliz, "postgres" -> Postgres
  | Postgres, _ -> Querysubj
  | Querysubj, "samples" -> Samples
  | Samples, s when is_float s -> Sample
  | Sample, _ -> Samples
  | Samples, _ -> Querysubj
  | Querysubj, "apps" -> Apps
  | Apps, _ -> Appname
  | Appname, _ -> Apps
  | Querysubj, "users" -> Users
  | Users, "users" -> User
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
  | Root, _ -> Root
  | _ -> state (* On reste dans le même état, endless loop*);;


let h = Hashtbl.create 786;;

let rec process_json d h lastname state =
  match Jsonm.decode d with
  | `Lexeme l ->
      (match l with
       | `Null -> Printf.printf "Lexeme: Null\n"
       | `Bool b -> Printf.printf "Lexeme: Bool - %b\n" b; H.add h lastname (b |> string_of_bool);
       | `String s -> Printf.printf "Lexeme: String - %s\n" s; H.add h lastname s;
       | `Float f -> Printf.printf "Lexeme: Float - %f\n" f; H.add h lastname (f |> string_of_float);
       | `Name n -> Printf.printf "Lexeme: Name - %s\n" n
       | `Os -> Printf.printf "Lexeme: START of Object\n"; 
       | `Oe -> Printf.printf "Lexeme: END of Object\n"; 
       | `As -> Printf.printf "Lexeme: Start of Array\n"
       | `Ae -> Printf.printf "Lexeme: End of Array\n");
      let curname, new_state = match l with
        | `Name n -> n, state 
        | `Os -> lastname, transition state lastname 
        | `Oe -> lastname, previous_state state 
        | _ -> lastname, state in
      Printf.printf "STATE: %s\n\n"  (printState new_state);
      process_json d h curname new_state
  | `End -> Printf.printf "Completed JSON parsing\n"
  | `Error e -> failwith (Format.asprintf "decode_value : Decode error: %a" Jsonm.pp_error e)
  | `Await -> failwith "Unexpected `Await in decoder";;



let read_and_process_json json_input =
  let d = Jsonm.decoder (`String json_input) in
  process_json  d h "root" Root;;     
