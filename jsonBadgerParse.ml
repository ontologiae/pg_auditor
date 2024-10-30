(*#require "jsonm, batteries";;*)
open Jsonm2json;;
module H = BatHashtbl;;
module L = BatList;;
module O = BatOption;;
module A = BatArray;;
module S = BatString;;

type sample = {
  remote: string option;
  db: string;
  plan: string option;
  bind: string option;
  query_wparam: string option;
  date: string option;
  user: string option;
  app: string option;
  timestamp : string option;
}
type user_app_info = {
  total_duration: float option;
  count: int option;
}



type chronos_hour_info = {
  count: int option;
  total_duration: float option;
  minutes_duration: (float * float) list;
  minutes: (float * int) list ;
}


type chronos_info = {
  date : (string * (int, chronos_hour_info) Hashtbl.t ) list
}


type query_info = {
  total_duration: float option;
  samples: (float, sample) Hashtbl.t;
  count: int option;
  apps: (string, user_app_info) Hashtbl.t;
  max: float option;
  users: (string, user_app_info) Hashtbl.t;
  chronos: chronos_info option;
  min: float option;
  query:string;
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
        | `Null -> Printf.eprintf "Lexeme: Null\n"
        | `Bool b -> Printf.eprintf "Lexeme: Bool - %b\n" b
        | `String s -> Printf.eprintf "Lexeme: String - %s\n" s
        | `Float f -> Printf.eprintf "Lexeme: Float - %f\n" f
        | `Name n -> Printf.eprintf "Lexeme: Name - %s\n" n
        | `Os -> Printf.eprintf "Lexeme: Start of Object\n"
        | `Oe -> Printf.eprintf "Lexeme: End of Object\n"
        | `As -> Printf.eprintf "Lexeme: Start of Array\n"
        | `Ae -> Printf.eprintf "Lexeme: End of Array\n"
      end;
      print_lexemes d
  | `End -> Printf.eprintf "Lexeme: End of JSON\n"
  | `Error e -> failwith (Format.asprintf "decode_value : Decode error: %a" Jsonm.pp_error e)
  | `Await -> failwith "Unexpected `Await in decoder";;




let print_decoded_json_string json_str =
  let d = Jsonm.decoder (`String json_str) in
  print_lexemes d;;



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
  Printf.eprintf "STATE: %s\n\n"  (string_of_state state)
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








 let get n l =
         
         let e = List.assoc n l in 
         match e with
         | String s      -> s
         | Float f       -> string_of_float f
         | Bool true     -> "true"
         | Bool false    -> "false"
         | _ -> failwith ("get n l match failure sur n="^n);;
       (*  | json -> Tiny_json.Json.as_string json |> failwith  ;;*)


 let getStr n l =
         try
          (  let e = L.assoc n l in 
         match e with
         | String s      -> Some s
         | Float f       -> Some (string_of_float f)
         | Bool true     -> Some "true"
         | Bool false    -> Some "false"
         | _ ->  (*Printf.eprintf "getStr n l match failure sur n=%s\n" n;*) None)
        with e -> Printf.eprintf "getStr L.assoc Not_found"; None;;


 let getFloat n l =
         try 
         (let e = L.assoc n l in 
         match e with
         | Float f      -> Some f
         | _            -> None)
         with e -> Printf.eprintf "getFloat L.assoc Not_found"; None;;

 let getInt n l =
         try 
         (let e = L.assoc n l in 
         match e with
         | Float f      -> Some (f |> int_of_float)
         | _            -> None)
         with e -> Printf.eprintf "getFloat L.assoc Not_found"; None;;


 let toSample json =
         let existName n l = List.exists (fun (a,b) -> a = n) l in
         let getCropedStr n l  = getStr n l |> O.map (fun s -> S.left s 4095) in
         match json with
        |  Obj l when existName "date" l && existName "query" l ->
                                 let get n = get n l in
                                {  remote = getCropedStr "remote" l;
                                   db = get "db";
                                   plan = getCropedStr "plan" l;
                                   bind = getCropedStr "bind" l;
                                   query_wparam = getCropedStr "query" l ; (*On crop les requêtes trop longues*)
                                   date = getCropedStr "date" l;
                                   user = getCropedStr "user" l;
                                   timestamp = getStr "date" l;
                                   app = getCropedStr "app" l }
        | _ -> failwith "ce n'est pas un Sample";;

 let toSamples jsonc h =
        match jsonc with
        | "samples", Obj l -> List.iter (fun (temps,l) -> H.add h temps  (toSample l)) l
        | _ -> failwith "Pas un couple samples";;



let toUserAppInfo json =
  match json with
  | Obj l -> 
    let get n = getFloat n l in
    { total_duration = getFloat "duration" l;
      count =  getInt "count" l }
  | _ -> failwith "ce n'est pas un UserAppInfo";;


let toUserAppInfos json h =
  match json with
  | "users", Obj l | "apps", Obj l -> List.iter (fun (name, obj) -> H.add h name (toUserAppInfo obj)) l
  | _ -> failwith "Pas un couple users ou apps";;

let toMinDurationInfo json =
  match json with
  | Float f ->  f 
  | _ -> failwith "ce n'est pas un MinDurationInfo";;

let toMinDurationInfos json  =
  match json with
  | "min_duration", Obj l -> L.map (fun (time, obj) -> (float_of_string time), (toMinDurationInfo obj)) l
  | _ -> failwith "Pas un couple min_duration";;

let toMinInfo json =
  match json with
  | Float f -> int_of_float f 
  | _ -> failwith "ce n'est pas un MinInfo";;

let toMinInfos json  =
  match json with
  | "min", Obj l -> L.map (fun (time, obj) -> (float_of_string time), (toMinInfo obj)) l
  | _ -> failwith "Pas un couple min";;


(*TODO bug : les chronos ne sont pas lus
List.map (fun a -> H.to_list a.chronos.day_info) q;;
pour tester 
 *)

open Either;;

let rec zip lst1 lst2 =
  match lst1, lst2 with
  | [], _ | _, [] -> []
  | (a1, b1) :: xs1, (a2, c2) :: xs2 ->
    if a1 = a2 then (a1, (b1, c2)) :: zip xs1 xs2
    else if a1 < a2 then zip xs1 lst2
    else zip lst1 xs2;;




let toChronosHourInfo json =
  match json with
  | Obj l -> 
    let get n = get n l in
    let resLst : ((float * float) list, (float * int) list) Either.t option list =  List.map (fun field -> match field with
                                        | "min_duration", Obj _ ->  Some(Left(toMinDurationInfos field ))
                                        | "min", Obj _ -> Some(Right(toMinInfos field ))
                                          | _ -> None) l in
    (*Printf.eprintf "resLst %d éléments\n%!" (L.length resLst);*)
    { count = getInt "count" l;
      total_duration = getFloat "duration" l;
      minutes_duration = L.filter_map (fun lr -> match lr with | Some(Left(dur)) -> Some(dur) | _ -> None) resLst |> L.flatten ;
      minutes = L.filter_map (fun lr -> match lr with | Some(Right(count)) -> Some(count) | _ -> None) resLst  |> L.flatten }
  | _ -> failwith "ce n'est pas un ChronosHourInfo";;

let toChronosHourInfos json h =
  match json with
  | Obj l -> List.iter (fun (heure, obj) -> let heure = int_of_string heure  in
                                            if heure >= 1 && heure < 25 then H.add h heure (toChronosHourInfo obj)) l
  | _ -> failwith "Pas un couple hours";;

(*let toChronosDayInfo json =
  match json with
  | Obj l -> 
    let hours = H.create 24 in
    List.iter (fun field -> match field with
      | hoursname, obj when is_int hoursname -> toChronosHourInfos obj hours
      | _ -> ()) l;
    { hours = hours }
  | _ -> failwith "ce n'est pas un ChronosDayInfo";;
*)


let toChronosDayInfos json h =
  match json with
  | Obj l -> List.iter (fun (heure, obj) -> let heure = int_of_string heure  in
                                            if heure >= 1 && heure < 25 then H.add h heure (toChronosHourInfo obj)) l
  | _ -> failwith "Pas un couple day_info";;




let toChronosInfo json =
  match json with
  | Obj l -> 
    let day_info = H.create 365 in
    let date = L.map (fun field -> match field with
      | yyyymmdd, obj when is_int yyyymmdd  && (S.length yyyymmdd = 8) -> 
                      let _ = toChronosDayInfos obj day_info in (*Chargement de la Hash*)
                      yyyymmdd, day_info
      | d, obj ->  "toChronosInfo ce n'est pas une date "^d |> failwith) l in
    { 
      date = date
    }
  | _ -> failwith "ce n'est pas un ChronosInfo";;




let toQueryInfo json =
  match json with
  | Obj [(query,Obj l)] -> 
    let get n = get n l in
    let samples = H.create 10 in
    let apps = H.create 10 in
    let users = H.create 10 in
    let chronos = ref None in
    List.iter (fun field -> match field with
      | "samples", Obj _ -> toSamples field samples
      | "apps", Obj _ -> toUserAppInfos field apps
      | "users", Obj _ -> toUserAppInfos field users
      | "chronos", obj -> chronos := Some(toChronosInfo obj)
      | _ -> ()) l;
    { total_duration = getFloat "duration" l;
                                                                (*On a des cas où le nom de l'objet est une chaine vide !*)
      samples = H.to_list samples |> List.map (fun (a,b) -> (try float_of_string a with e -> 0.),b) |> H.of_list;
      count = O.map int_of_float  (getFloat "count" l) ;
      apps = apps;
      max =  getFloat "max" l;
      users = users;
      chronos = !chronos;
      min = getFloat "min" l ;
      query = query;
      }
  | _ -> failwith "ce n'est pas un QueryInfo";;




















let rec process_json d lastname state acc =
        let dec = Jsonm.decode d in
        let print l = match l with
       | `Null -> Printf.eprintf "Lexeme: Null\n"
       | `Bool b -> Printf.eprintf "Lexeme: Bool - %b\n" b; 
       | `String s -> Printf.eprintf "Lexeme: String - %s\n" s; 
       | `Float f -> Printf.eprintf "Lexeme: Float - %f\n" f; 
       | `Name n -> Printf.eprintf "Lexeme: Name - %s\n" (S.left n 20)
       | `Os -> Printf.eprintf "Lexeme: START of Object\n"; 
       | `Oe -> Printf.eprintf "Lexeme: END of Object\n"; 
       | `As -> Printf.eprintf "Lexeme: Start of Array\n"
       | `Ae -> Printf.eprintf "Lexeme: End of Array\n" in
        (*printState state;*)
        match dec with
          | `Lexeme l ->  begin 
                           (*print l;*)
                           match l, state with
                                | `Name n, _ -> process_json d n state acc
                                | `Os, Postgres ->
                                                (*let newstate = transition state lastname in
                                                        printState newstate;*)
                                        process_json d lastname state ((parse_jsonm d lastname)::acc) 
                                | `Os, n -> let newstate = transition state lastname in  process_json d lastname newstate acc
                                | `Oe, _ -> let newstate = previous_state state in (* printState newstate;*) process_json d lastname newstate acc
                                | _ ->  process_json d lastname state acc  
                          end
          | `End -> Printf.eprintf "Completed JSON parsing\n"; lastname, state, acc 
          | `Error e -> failwith (Format.asprintf "decode_value : Decode error: %a" Jsonm.pp_error e)
          | `Await -> failwith "Unexpected `Await in decoder";;



let read_and_process_json json_input =
  let d = Jsonm.decoder (`String json_input) in
  process_json  d  "root" Root [];; 



let readfile_and_process_json_file file_path =
  let ic = open_in file_path in
  (*let json_input = really_input_string ic (in_channel_length ic) in*)
  
  let d = Jsonm.decoder (`Channel ic) in
  let res = process_json d "root" Root [] in
  close_in ic;
  res;;

let rec process_json_unit d h lastname state =
        let dec = Jsonm.decode d in
  match dec with
  | `Lexeme l ->
      (match l with
       | `Null -> Printf.eprintf "Lexeme: Null\n"
       | `Bool b -> Printf.eprintf "Lexeme: Bool - %b\n" b; H.add h lastname (b |> string_of_bool);
       | `String s -> Printf.eprintf "Lexeme: String - %s\n" s; H.add h lastname s;
       | `Float f -> Printf.eprintf "Lexeme: Float - %f\n" f; H.add h lastname (f |> string_of_float);
       | `Name n -> Printf.eprintf "Lexeme: Name - %s\n" n
       | `Os -> Printf.eprintf "Lexeme: START of Object\n"; 
       | `Oe -> Printf.eprintf "Lexeme: END of Object\n"; 
       | `As -> Printf.eprintf "Lexeme: Start of Array\n"
       | `Ae -> Printf.eprintf "Lexeme: End of Array\n");
      let curname, new_state = match l, state with
        | `Name n, _ -> n, state 
        | `Os, _ -> lastname, transition state lastname 
        | `Oe, Root -> lastname, previous_state state 
        | _ -> lastname, state in
      printState new_state;
      process_json_unit d h curname new_state
  | `End -> Printf.eprintf "Completed JSON parsing\n"
  | `Error e -> failwith (Format.asprintf "decode_value : Decode error: %a" Jsonm.pp_error e)
  | `Await -> failwith "Unexpected `Await in decoder";;


let parse_json_to_query_info file_path =
        let _,_,jsona = readfile_and_process_json_file file_path in
        List.map  toQueryInfo jsona;;


(* * Fonctions d'analyses des Query_infos* *)


(*Renvoi la 1ére requête issue du sample, donc parsable puisque requête réelle*)
let getOneParsableQuery q =
        try Some((H.to_list q.samples |> List.hd |> snd).query_wparam) with e -> prerr_endline "getOneParsableQuery, pas de requêtes dans samples"; None;;

