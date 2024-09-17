Voici un extrait de JSON :
```json
 "normalyzed_info" : {
      "postgres" : {
"SELECT From Where" : {
            "duration" : 0.826,
            "samples" : {
               "0.167" : {
                  "remote" : null,
                  "db" : "adep",
                  "plan" : null,
                  "bind" : null,
                  "query" : "SELECT From Where",
                  "date" : "2024-06-06 23:09:18",
                  "user" : "openassur",
                  "app" : "[unknown]"
               },
               "0.184" : {
                  "plan" : null,
                  "db" : "adep",
                  "bind" : null,
                  "remote" : null,
                  "app" : "[unknown]",
                  "user" : "openassur",
                  "date" : "2024-06-06 23:08:16",
                  "query" : "SELECT From Where"
               },
               "0.312" : {
                  "remote" : null,
                  "plan" : null,
                  "db" : "adep",
                  "bind" : null,
                  "date" : "2024-06-06 23:09:18",
                  "query" : "SELECT From Where",
                  "app" : "[unknown]",
                  "user" : "myuser"
               }
            },
            "count" : 4,
            "apps" : {
               "[unknown]" : {
                  "count" : 4,
                  "duration" : 0.826
               }
            },
            "max" : "0.312",
            "users" : {
               "myuser" : {
                  "duration" : 0.826,
                  "count" : 4
               }
            },
            "chronos" : {
               "20240606" : {
                  "23" : {
                     "count" : 4,
                     "duration" : 0.826,
                     "min_duration" : {
                        "08" : 0.347,
                        "09" : 0.479
                     },
                     "min" : {
                        "09" : 2,
                        "08" : 2
                     }
                  }
               }
            },
            "min" : "0.163"
         }
	}
}```

Voici le type Ocaml permettant d'enregistrer les données de ce JSON
```ocaml
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
type app_info = {
  count: int;
  duration: float;
}
type user_info = {
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
  apps: (string, app_info) Hashtbl.t;
  max: float;
  users: (string, user_info) Hashtbl.t;
  chronos: chronos_info;
  min: float;
}

type root = {
  queryProto : query_info list;
}
```

Voici le fichier mli de Jsonm
```jsonm
(** Non-blocking streaming JSON codec.
    [Jsonm] is a non-blocking streaming codec to
    {{!section:decode}decode} and {{!section:encode}encode} the
    {{:http://tools.ietf.org/html/rfc7159}JSON} data format. It can
    process JSON text without blocking on IO and without a complete
    in-memory representation of the data.
    The {{!Uncut}uncut codec} also processes whitespace and
    (non-standard) JSON with JavaScript comments.
 {1:datamodel JSON data model} *)
type lexeme = [
| `Null
| `Bool of bool
| `String of string
| `Float of float
| `Name of string
| `As
| `Ae
| `Os
| `Oe ]
(** The type for JSON lexemes. [`As] and [`Ae]
    start and end arrays and [`Os] and [`Oe] start
    and end objects. [`Name] is for the member names of objects.
    A {e well-formed} sequence of lexemes belongs to the language of
    the [json] grammar:
{[
  json = value 
object = `Os *member `Oe
member = (`Name s) value
 array = `As *value `Ae
 value = `Null / `Bool b / `Float f / `String s / object / array
]}
 . *)
val pp_lexeme : Format.formatter -> [< lexeme] -> unit
(** [pp_lexeme ppf l] prints a unspecified non-JSON representation of [l]
    on [ppf]. *)
(** {1:decode Decode} *)
type error = [
| `Illegal_BOM
| `Illegal_escape of
    [ `Not_hex_uchar of Uchar.t
    | `Not_esc_uchar of Uchar.t
    | `Not_lo_surrogate of int
    | `Lone_lo_surrogate of int
    | `Lone_hi_surrogate of int ]
| `Illegal_string_uchar of Uchar.t
| `Illegal_bytes of string
| `Illegal_literal of string
| `Illegal_number of string
| `Unclosed of [ `As | `Os | `String | `Comment ]
| `Expected of
    [ `Comment | `Value | `Name | `Name_sep | `Json | `Eoi
    | `Aval of bool (* [true] if first array value  *)
    | `Omem of bool (* [true] if first object member *) ]]
(** The type for decoding errors. *)
val pp_error : Format.formatter -> [< error] -> unit
(** [pp_error e] prints an unspecified UTF-8 representation of [e] on [ppf]. *)
type encoding = [ `UTF_8 | `UTF_16 | `UTF_16BE | `UTF_16LE ]
(** The type for Unicode encoding schemes. *)
type src = [ `Channel of in_channel | `String of string | `Manual ]
(** The type for input sources. With a [`Manual] source the client
    must provide input with {!Manual.src}. *)
type decoder
(** The type for JSON decoders. *)
val decoder :?encoding:[< encoding] -> [< src] -> decoder
(** [decoder encoding src] is a JSON decoder that inputs from [src].
    [encoding] specifies the character encoding of the data. If unspecified
    the encoding is guessed as
    {{:http://tools.ietf.org/html/rfc4627#section-3}suggested} by
    the old RFC4627 standard. *)
val decode : decoder -> [> `Await | `Lexeme of lexeme | `End | `Error of error ]
(** [decode d] is:
    {ul
    {- [`Await] if [d] has a [`Manual] source and awaits for more input.
       The client must use {!Manual.src} to provide it.}
    {- [`Lexeme l] if a lexeme [l] was decoded.}
    {- [`End] if the end of input was reached.}
    {- [`Error e] if a decoding error occured. If the client is interested
       in a best-effort decoding it can still continue to decode
       after an error (see {!errorrecovery}) although the resulting sequence
       of [`Lexeme]s is undefined and may not be well-formed.}}
    The {!Uncut.pp_decode} function can be used to inspect decode results.
    {b Note.} Repeated invocation always eventually returns [`End], even
    in case of errors. *)
val decoded_range : decoder -> (int * int) * (int * int)
(** [decoded_range d] is the range of characters spanning the last
    [`Lexeme] or [`Error] (or [`White] or [`Comment] for an
    {!Uncut.decode}) decoded by [d].  A pair of line and column numbers
    respectively one and zero based. *)
val decoder_encoding : decoder -> encoding
(** [decoder_encoding d] is [d]'s encoding.
    {b Warning.} If the decoder guesses the encoding, rely on this
    value only after the first [`Lexeme] was decoded. *)
val decoder_src : decoder -> src
(** [decoder_src d] is [d]'s input source. *)
(** {1:encode Encode} *)
type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]
(** The type for output destinations. With a [`Manual] destination the
    client must provide output storage with {!Manual.dst}. *)
type encoder
(** The type for JSON encoders. *)
val encoder : ?minify:bool -> [< dst] -> encoder
(** [encoder minify dst] is an encoder that outputs to [dst]. If
    [minify] is [true] (default) the output is made as compact as
    possible, otherwise the output is indented. If you want better
    control on whitespace use [minify = true] and {!Uncut.val-encode}. *)
val encode : encoder -> [< `Await | `End | `Lexeme of lexeme ] ->
  [`Ok | `Partial]
(** [encode e v] is:
    {ul
    {- [`Partial] iff [e] has a [`Manual] destination and needs more
       output storage. The client must use {!Manual.dst} to provide
       a new buffer and then call {!val-encode} with [`Await] until [`Ok]
       is returned.}
    {- [`Ok] when the encoder is ready to encode a new [`Lexeme]
       or [`End].}}
    For [`Manual] destinations, encoding [`End] always returns [`Partial],
    the client should as usual use {!Manual.dst} and continue with [`Await]
    until [`Ok] is returned at which point {!Manual.dst_rem} [e] is guaranteed
    to be the size of the last provided buffer (i.e. nothing was written).
    {b Raises.} [Invalid_argument] if a non {{!datamodel}well-formed}
    sequence of lexemes is encoded or if [`Lexeme] or [`End] is
    encoded after a [`Partial] encode. *)
val encoder_dst : encoder -> dst
(** [encoder_dst e] is [e]'s output destination. *)
val encoder_minify : encoder -> bool
(** [encoder_minify e] is [true] if [e]'s output is minified. *)
(** {1:manual Manual sources and destinations} *)
(** Manual input sources and output destinations.
``` 
à partir de l'exemple de fichier Json, de la définition de type ocaml et du fichier mli décrivant le fonctionnement de la librairie Jsonm version 1.0.2, écrit un parser utilisant cette librairie pour parser le Json







----------------------------

Bien sûr! Concentrons-nous sur le processus de parsing (décodage) JSON avec la bibliothèque Jsonm en OCaml. Voici un tutoriel axé sur les étapes essentielles pour décoder du JSON en utilisant Jsonm.

### Installation

Pour installer Jsonm, vous pouvez utiliser OPAM :

```sh
opam install jsonm
```

### Importation

Commencez par importer Jsonm dans votre module OCaml :

```ocaml
open Jsonm
```

### Lexèmes JSON

Jsonm traite les JSON sous forme de lexèmes. Voici quelques principaux types de lexèmes (`lexeme`) que Jsonm utilise :

- `Os` et `Oe` (début et fin d'un objet)
- `As` et `Ae` (début et fin d'un tableau)
- `Name` (nom d'une paire clé-valeur dans un objet)
- `String`, `Float`, `Bool`, `Null` (différents types de valeurs)

### Décoder un JSON d'une chaîne de caractères

Pour décoder un JSON, nous devons lire les lexèmes successifs jusqu'à ce que nous ayons traité la totalité du contenu JSON.

#### Exemple simple : Décoder depuis une chaîne

Supposons que nous ayons une chaîne JSON simple à décoder.

```ocaml
let json_string = "{ \"key\": \"value\", \"number\": 123, \"array\": [true, false, null] }"

let decode_from_string json_str =
  let d = Jsonm.decoder (`String json_str) in
  let rec process () =
    match Jsonm.decode d with
    | `Lexeme l -> 
        (* Traiter le lexème ici *)
        Format.printf "Lexeme: %a\n" Jsonm.pp_lexeme l;
        process ()
    | `End -> 
        Format.printf "Fin du décodage.\n"
    | `Error e ->
        Format.printf "Erreur de décodage: %a\n" Jsonm.pp_error e
    | _ -> 
        failwith "Unexpected condition during decoding"
  in
  process ()

let () = decode_from_string json_string
```

Ici, la fonction `Jsonm.decode` est appelée de manière récursive pour traiter chaque lexème du JSON jusqu'à ce que la fin (`End`) soit atteinte.

### Traiter et Construire une Structure OCaml

Supposons maintenant que nous voulions décoder un JSON en une structure de données OCaml. Par exemple, décodons un JSON en utilisant un type de données représentant un objet avec des champs de différentes sortes.

#### Définir des types pour les données décodées

Définissons les types nécessaires :

```ocaml
type json_value =
  | Null
  | Bool of bool
  | Float of float
  | String of string
  | Object of (string * json_value) list
  | Array of json_value list
```

#### Fonction de décodage JSON

Écrivons une fonction pour transformer les lexèmes en valeur OCaml:

```ocaml
let rec decode_json_value d =
  match Jsonm.decode d with
  | `Lexeme (`Null) -> Null
  | `Lexeme (`Bool b) -> Bool b
  | `Lexeme (`Float f) -> Float f
  | `Lexeme (`String s) -> String s
  | `Lexeme `Os -> decode_json_object d
  | `Lexeme `As -> decode_json_array d
  | `Error e -> failwith (Format.asprintf "Erreur de décodage: %a" Jsonm.pp_error e)
  | _ -> failwith "Unexpected JSON structure"

and decode_json_object d =
  let rec loop acc =
    match Jsonm.decode d with
    | `Lexeme `Oe -> Object (List.rev acc)
    | `Lexeme (`Name k) ->
        let v = decode_json_value d in
        loop ((k, v) :: acc)
    | _ -> failwith "Expected JSON object key or end"
  in
  loop []

and decode_json_array d =
  let rec loop acc =
    match Jsonm.decode d with
    | `Lexeme `Ae -> Array (List.rev acc)
    | `Lexeme _ as l ->
        let v = decode_json_value d in
        loop (v :: acc)
    | _ -> failwith "Expected JSON array element or end"
  in
  loop []

let decode_json_string json_str =
  let d = Jsonm.decoder (`String json_str) in
  decode_json_value d

(* Example usage *)
let () =
  let json_str = "{ \"key\": \"value\", \"number\": 123, \"array\": [true, false, null] }" in
  let result = decode_json_string json_str in
  match result with
  | Object _ -> Printf.printf "Decoded an object.\n"
  | _ -> Printf.printf "Decoded something else.\n"
```

### Conclusion

Ce tutoriel de base sur Jsonm montre comment décoder des JSON à partir de chaînes pour les transformer en structures de données OCaml. Vous pouvez appliquer cette technique pour décoder des JSON plus complexes en définissant des types et en écrivant des fonctions de décodage correspondant à la structure des données JSON attendue.

Jsonm est puissant pour traiter des flux JSON de manière non bloquante, ce qui est utile pour des applications nécessitant des traitements efficaces en termes de mémoire et de performance.









-----------------------------------------------
Montre moi différentes façon de modéliser une machine à état en Ocaml, de sorte à s'assurer, dans le programme, qu'on respecte les transitions possibles
Montre différentes manières, élégantes, pratiques, ou autres

En OCaml, il existe plusieurs façons élégantes et pratiques de modéliser une machine à états pour s'assurer que les transitions possibles sont respectées. Voici quelques méthodes :
### 1. Utilisation de variantes (variants) et de `match`
On peut utiliser des variantes pour définir les états de la machine et utiliser `match` pour s'assurer que les transitions sont respectées.
```ocaml
type state =
  | State1
  | State2
  | State3
type event =
  | EventA
  | EventB
let transition state event =
  match state, event with
  | State1, EventA -> State2
  | State2, EventB -> State3
  | State3, EventA -> State1
  | _ -> failwith "Invalid transition"
(* Usage *)
let () =
  let current_state = State1 in
  let next_state = transition current_state EventA in
  Printf.printf "Next state: %s\n" (match next_state with
                                    | State1 -> "State1"
                                    | State2 -> "State2"
                                    | State3 -> "State3")
```
### 2. Utilisation de modules et de types abstraits
Les modules peuvent encapsuler les états et les transitions, offrant une manière plus modulaire et sécurisée.
```ocaml
module type STATE_MACHINE = sig
  type state
  type event
  val init : state
  val transition : state -> event -> state
end
module MyStateMachine : STATE_MACHINE = struct
  type state = State1 | State2 | State3
  type event = EventA | EventB
  let init = State1
  let transition state event =
    match state, event with
    | State1, EventA -> State2
    | State2, EventB -> State3
    | State3, EventA -> State1
    | _ -> failwith "Invalid transition"
end
(* Usage *)
let () =
  let open MyStateMachine in
  let current_state = init in
  let next_state = transition current_state EventA in
  Printf.printf "Next state: %s\n" (match next_state with
                                    | State1 -> "State1"
                                    | State2 -> "State2"
                                    | State3 -> "State3")
```
### 3. Utilisation de GADT (Generalized Algebraic Data Types)
Les GADT permettent de capturer des propriétés plus fines des transitions et d'ajouter des vérifications de type plus strictes.
```ocaml
type _ state =
  | State1 : State2 state state
  | State2 : State3 state
  | State3 : State1 state state
and 'a state = ..
type _ event =
  | EventA : State1 state
  | EventB : State2 state
let transition : type a. a state -> a event -> a = fun state event ->
  match state, event with
  | State1, EventA -> State2
  | State2, EventB -> State3
  | State3, EventA -> State1
  | _ -> failwith "Invalid transition"
(* Usage *)
let () =
  let current_state = State1 in
  let next_state = transition current_state EventA in
  Printf.printf "Next state: %s\n" (match next_state with
                                    | State1 -> "State1"
                                    | State2 -> "State2"
                                    | State3 -> "State3")
```
### 4. Utilisation de types polymorphes et de variantes ouvertes
Vous pouvez utiliser des variantes ouvertes pour ajouter de la flexibilité à votre machine à états.
  
```ocaml
type state =
  [ `State1 | `State2 | `State3 ]
type event =
  [ `EventA | `EventB ]
let transition (state : state) (event : event) : state =
  match state, event with
  | `State1, `EventA -> `State2
  | `State2, `EventB -> `State3
  | `State3, `EventA -> `State1
  | _ -> failwith "Invalid transition"
(* Usage *)
let () =
  let current_state : state = `State1 in
  let next_state = transition current_state `EventA in
  Printf.printf "Next state: %s\n" (match next_state with
                                    | `State1 -> "State1"
                                    | `State2 -> "State2"
                                    | `State3 -> "State3")
```
Chacune de ces méthodes offre une approche différente avec ses propres avantages. Les variations dans la complexité et les garanties de type permettent de choisir la méthode la mieux adaptée à vos besoins spécifiques.
