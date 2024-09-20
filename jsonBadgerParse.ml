#require "jsonm, batteries";;

module H = BatHashtbl;;

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
                  \"db\" : \"sdqsqdsdqs\",
                  \"plan\" : null,
                  \"bind\" : null,
                  \"query\" : \"SELECT From Where\",
                  \"date\" : \"2024-06-06 23:09:18\",
                  \"user\" : \"sdqqdsqdqs\",
                  \"app\" : \"[unknown]\"
               },
               \"0.184\" : {
                  \"plan\" : null,
                  \"db\" : \"aped\",
                  \"bind\" : null,
                  \"remote\" : null,
                  \"app\" : \"[unknown]\",
                  \"user\" : \"assuropen\",
                  \"date\" : \"2024-06-06 23:08:16\",
                  \"query\" : \"SELECT From Where\"
               },
               \"0.312\" : {
                  \"remote\" : null,
                  \"plan\" : null,
                  \"db\" : \"aped\",
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
         },


\"SELECT From Where 2222222\" : {
            \"duration\" : 99.826,
            \"samples\" : {
               \"90.167\" : {
                  \"remote\" : null,
                  \"db\" : \"adqsddqsdsqdep\",
                  \"plan\" : null,
                  \"bind\" : null,
                  \"query\" : \"SELECsdqdqsdsqT From Where\",
                  \"date\" : \"2024-06-06 23:09:18\",
                  \"user\" : \"assuropen\",
                  \"app\" : \"[unknown]\"
               },
               \"90.184\" : {
                  \"plan\" : null,
                  \"db\" : \"adsqddep\",
                  \"bind\" : null,
                  \"remote\" : null,
                  \"app\" : \"[unknown]\",
                  \"user\" : \"assuropen\",
                  \"date\" : \"2024-06-06 23:08:16\",
                  \"query\" : \"SELECT From Where\"
               },
               \"90.312\" : {
                  \"remote\" : null,
                  \"plan\" : null,
                  \"db\" : \"dqsdqsdqsd\",
                  \"bind\" : null,
                  \"date\" : \"2024-06-06 23:09:18\",
                  \"query\" : \"SELECT From Where\",
                  \"app\" : \"[unknown]\",
                  \"user\" : \"myuser\"
               }
            },
            \"count\" : 54,
            \"apps\" : {
               \"[unknown]\" : {
                  \"count\" : 44,
                  \"duration\" : 40.826
               }
            },
            \"max\" : \"440.312\",
            \"users\" : {
               \"myuser\" : {
                  \"duration\" : 40.826,
                  \"count\" : 94
               }
            },
            \"chronos\" : {
               \"20240606\" : {
                  \"23\" : {
                     \"count\" : 94,
                     \"duration\" : 90.826,
                     \"min_duration\" : {
                        \"08\" : 80.347,
                        \"09\" : 80.479
                     },
                     \"min\" : {
                        \"09\" : 52,
                        \"18\" : 52
                     }
                  }
               }
            },
            \"min\" : \"50.163\"
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



let rec printState state =
  Printf.printf "STATE: %s\n\n"  (string_of_state state)
 and string_of_state   state =     
        match state with
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
  | other -> string_of_state other |> failwith;;


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
  | Root, _ -> Root
  | _ -> state (* On reste dans le même état, endless loop*);;


type json =
  | Null
  | Bool of bool
  | Float of float
  | String of string
  | List of json list
  | Obj of (string * json) list;;



let rec parse_jsonm d =
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
  parse_value ();;




let rec process_json d lastname state acc =
        let dec = Jsonm.decode d in
        let print l = match l with
       | `Null -> Printf.printf "Lexeme: Null\n"
       | `Bool b -> Printf.printf "Lexeme: Bool - %b\n" b; 
       | `String s -> Printf.printf "Lexeme: String - %s\n" s; 
       | `Float f -> Printf.printf "Lexeme: Float - %f\n" f; 
       | `Name n -> Printf.printf "Lexeme: Name - %s\n" n
       | `Os -> Printf.printf "Lexeme: START of Object\n"; 
       | `Oe -> Printf.printf "Lexeme: END of Object\n"; 
       | `As -> Printf.printf "Lexeme: Start of Array\n"
       | `Ae -> Printf.printf "Lexeme: End of Array\n" in
        printState state;
        match dec with
          | `Lexeme l ->  begin 
                           print l;
                           match l, state with
                                | `Name n, _ -> process_json d n state acc
                                | `Os, Postgres -> printState state;
                                                (*let newstate = transition state lastname in
                                                        printState newstate;*)
                                        process_json d lastname state ((parse_jsonm d)::acc) 
                                        (*TODO : ça merde parce que le parse_value doit lui tomber sur un `Os, or il est déjà lue, donc il tombe sur le token suivant
                                                        on a recopié le code de parse-object dans le cas name dans parse_value*)
                                | `Os, n -> let newstate = transition state lastname in printState newstate; process_json d lastname newstate acc
                                | `Oe, _ -> let newstate = previous_state state in  printState newstate; process_json d lastname newstate acc
                                | _ -> printState state; process_json d lastname state acc  
                          end
                          
          | `End -> Printf.printf "Completed JSON parsing\n"; lastname, state, acc 
          | `Error e -> failwith (Format.asprintf "decode_value : Decode error: %a" Jsonm.pp_error e)
          | `Await -> failwith "Unexpected `Await in decoder";;



let read_and_process_json json_input =
  let d = Jsonm.decoder (`String json_input) in
  process_json  d  "root" Root [];; 


let rec process_json_unit d h lastname state =
        let dec = Jsonm.decode d in
  match dec with
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
      let curname, new_state = match l, state with
        | `Name n, _ -> n, state 
        | `Os, _ -> lastname, transition state lastname 
        | `Oe, Root -> lastname, previous_state state 
        | _ -> lastname, state in
      printState new_state;
      process_json_unit d h curname new_state
  | `End -> Printf.printf "Completed JSON parsing\n"
  | `Error e -> failwith (Format.asprintf "decode_value : Decode error: %a" Jsonm.pp_error e)
  | `Await -> failwith "Unexpected `Await in decoder";;




