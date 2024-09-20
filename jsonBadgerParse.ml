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


type acumulateur = {
  mutable query_info: query_info option;
  mutable samples: (float, sample) Hashtbl.t;
  mutable apps: (string, user_app_info) Hashtbl.t;
  mutable users: (string, user_app_info) Hashtbl.t;
  mutable chronos: chronos_info option;
  mutable current_sample: sample option;
  mutable current_user_app_info: user_app_info option;
  mutable current_chronos_hour_info: chronos_hour_info option;
}

let accBase = {
        query_info = None;
        samples = Hashtbl.create 786;
        apps = Hashtbl.create 786;
        users = Hashtbl.create 787;
        chronos = None;
        current_sample = None;
        current_user_app_info = None;
        current_chronos_hour_info = None;
};;

let h = Hashtbl.create 788;;

let printAndStore d h lastname =
  let lexeme = Jsonm.decode d in
  Printf.printf "printAndStore:   lastname=%s  \n%!" lastname;
  match lexeme with
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
       | `Ae -> Printf.printf "Lexeme: End of Array\n"); lexeme
  | `End -> Printf.printf "Completed JSON parsing\n"; lexeme
  | `Error e -> failwith (Format.asprintf "decode_value : Decode error: %a" Jsonm.pp_error e)
  | `Await -> failwith "Unexpected `Await in decoder";;


(* - : decoder ->
    (string, string) H.t -> string -> state -> string * state * 'a option *)
let rec getSample d h lastname state =
        let lexeme = printAndStore d h lastname in
        let _ = printState state |> Printf.printf "Sample:   lastname=%s  Etat = %s\n%!" lastname in
        match lexeme, state with
        | `Lexeme (`Name n), _ -> printAndStore d h n; getSample d h lastname Sample;
        | `Lexeme (`Os), _ ->  failwith "Erreur : Un objet Sample ne possède aucun objet"
        | `Lexeme (`Oe), Sample -> Printf.printf "getSample:Rendu\n%!"; let l, e, res = lastname, Samples,  Some {    
                                                                        remote = H.find_option h "remote"; 
                                                                        db = H.find_option h "db" |> BatOption.default "";
                                                                        plan = H.find_option h "plan";
                                                                        bind = H.find_option h "bind";
                                                                        query = H.find_option h "query" |> BatOption.default "";
                                                                        date = H.find_option h "date" |> BatOption.default "";
                                                                        user = H.find_option h "user" |> BatOption.default "";
                                                                        app = H.find_option h "app" |> BatOption.default "";
                                                                   }
                                   in H.clear h; l,e,res
        | `Lexeme l, eta -> printAndStore d h lastname; printState eta |> Printf.printf "Cas général etat=%s  \n%!"; 
                                printAndStore d h lastname; 
                                getSample d h lastname Sample
        | `End, _ -> Printf.printf "Completed JSON parsing\n"; lastname, state, None
        | `Error e, _ -> failwith (Format.asprintf "decode_value : Decode error: %a" Jsonm.pp_error e)
        | `Await, _ -> failwith "Unexpected `Await in decoder";;


let rec getSamples d h lastname state (hres : (float, sample) H.t) =
        let lexeme = printAndStore d h lastname in
        let _ = printState state |> Printf.printf "Samples:Etat = %s\n%!" in
        match lexeme, state with
        | `Lexeme (`Name n), _ -> printAndStore d h n; getSamples d h n Samples hres
        | `Lexeme (`Os), _ -> Printf.printf "getSamples Objet début lastname=%s\n%!" lastname;
                                let ln, sta, samp = getSample d h lastname Samples in
                                
                                getSamples d h lastname Samples hres
        | `Lexeme (`Oe), Sample -> (*H.add hres (lastname |> float_of_string) (samp |> Option.get);*)
                                        Printf.printf "getSamples:Rendu lastname=%s \n%!" lastname; getSamples d h lastname Samples hres
        | `Lexeme l, eta ->
                            printState eta |> Printf.printf "Cas général etat=%s  \n%!"; 
                            printAndStore d h lastname; 
                            getSamples d h lastname Samples hres
        | `End, _ -> Printf.printf "Completed JSON parsing\n"; lastname, state, hres
        | `Error e, _ -> failwith (Format.asprintf "decode_value : Decode error: %a" Jsonm.pp_error e)
        | `Await, _ -> failwith "Unexpected `Await in decoder";;



let samples = " {
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
            }";;


let d = Jsonm.decoder (`String samples);;


(*Est-ce que je récup les propriétés via la H ? Mais faut la vider
 H.clear h
 Et faut que la fonction rende le bon type, donc process_json doit rendre un acuumulateur*)

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
      let curname, new_state = match l, state with
        | `Name n, _ -> n, state 
        | `Os, _ -> lastname, transition state lastname 
        | `Oe, Root -> lastname, previous_state state 
        | _ -> lastname, state in
      Printf.printf "STATE: %s\n\n"  (printState new_state);
      process_json d h curname new_state
  | `End -> Printf.printf "Completed JSON parsing\n"
  | `Error e -> failwith (Format.asprintf "decode_value : Decode error: %a" Jsonm.pp_error e)
  | `Await -> failwith "Unexpected `Await in decoder";;


let read_and_process_json json_input =
  let d = Jsonm.decoder (`String json_input) in
  process_json  d h "root" Root;; 

(******************************************)

type acumulateur = {
  mutable query_info: query_info option;
  mutable samples: (float, sample) Hashtbl.t;
  mutable apps: (string, user_app_info) Hashtbl.t;
  mutable users: (string, user_app_info) Hashtbl.t;
  mutable chronos: chronos_info option;
  mutable current_sample: sample option;
  mutable current_user_app_info: user_app_info option;
  mutable current_chronos_hour_info: chronos_hour_info option;
}

let create_accumulateur () = {
  query_info = None;
  samples = Hashtbl.create 10;
  apps = Hashtbl.create 10;
  users = Hashtbl.create 10;
  chronos = None;
  current_sample = None;
  current_user_app_info = None;
  current_chronos_hour_info = None;
}

(*

let rec process_json d acc state stack =
  match Jsonm.decode d with
  | `Lexeme l ->
      let new_state, new_stack = match l, state with
        | `Name n, _ -> state, stack
        | `Os, _ -> transition state, state :: stack
        | `Oe, s :: ss -> s, ss
        | _ -> state, stack in
      begin
        match l, state with
        | `String s, Sample _ -> acc.current_sample <- Some { (Option.get acc.current_sample) with user = s }
        | `Float f, Sample _ -> acc.current_sample <- Some { (Option.get acc.current_sample) with duration = f }
        | `String s, User -> acc.current_user_app_info <- Some { (Option.get acc.current_user_app_info) with user = s }
        | `Float f, User -> acc.current_user_app_info <- Some { (Option.get acc.current_user_app_info) with duration = f }
        | `String s, Appname -> acc.current_user_app_info <- Some { (Option.get acc.current_user_app_info) with app = s }
        | _ -> ()
      end;
      process_json d acc new_state new_stack
  | `End -> acc
  | `Error e -> failwith (Format.asprintf "decode_value : Decode error: %a" Jsonm.pp_error e)
  | `Await -> failwith "Unexpected `Await in decoder"

(* Example usage *)

let parse json_string =
  let d = Jsonm.decoder (`String json_string) in
  let acc = create_accumulator () in
  process_json d acc Root []
*)
