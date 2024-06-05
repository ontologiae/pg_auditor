open Tiny_json.Json
open Pg_query
open JsonSqlParse;;


let sql2 = "
With g(n) as ( select generate_series(1,1000) )
SELECT to_char(e.edateevent, 'HH24hMI (dy DD/MM)'::text) AS edateevent,
    ep.phrase,
    gereencodingnainwak(replace(replace(replace(replace(replace(replace(replace(replace(replace(replace(replace(replace(ep.phrase::text, '%casex'::text, e.ecasex::text), '%casey'::text, e.ecasey::text), '%Nain2'::text, ('<i>'::text || COALESCE(vdn2.lib_nain, ''::character varying)::text) || '</i>'::text), '%Nain'::text, ('<i>'::text || COALESCE(vdn1.lib_nain, ''::character varying)::text) || '</i>'::text), '%objet'::text, COALESCE(o.nom, ''::character varying)::text), '%monde'::text, COALESCE(m.lib, ''::text)), '%pv'::text, COALESCE(e.epv::text, ''::text)), '%gr'::text, COALESCE(e.egrad, ''::text)), '%pc'::text, e.eptcote::text), '%ph'::text, e.ehont::text), '%xp'::text, e.exp::text), '%vh'::text, COALESCE(ovh.nom, ''::character varying)::text)) AS ephrase,
    m.lib AS mondefromdetect,
    e.etype,
    o.image,
    vdn1.lib_nain AS nain1,
    'couleur Nain'::text AS couln,
    vdnub4.lib_nain AS userb4,
    (now() - e.edateevent::timestamp with time zone) < '1 day'::interval AS recent,
    e.eidmde,
    e.edateevent AS datebrut
   FROM events e
     JOIN events_phrase ep ON ep.idevent = e.etype
     LEFT JOIN v_dernier_nom_nainshisto vdn1 ON e.eidnain1 = vdn1.idnh
     LEFT JOIN v_dernier_nom_nainshisto vdn2 ON e.eidnain2 = vdn2.idnh
     JOIN utilisateur u ON e.eiddetecteur = u.id
     JOIN v_dernier_nom vdnub4 ON vdnub4.id = u.id_nain
     LEFT JOIN monde m ON e.eidmde = m.id
     LEFT JOIN objet o ON o.id = e.eobj1
     LEFT JOIN objet ovh ON ovh.id = e.evh
  WHERE e.edateevent > (now() - '5 days'::interval) AND e.edateevent < (now() + '02:00:00'::interval)
  Group By e.etype
  Having e.etype = 42
  ORDER BY m.lib, e.edateevent DESC
  Limit 50 Offset 2;";;


let sql_index = "
CREATE INDEX \"AllDetect2\" ON public.detects USING btree (didnain, ddatecreat, dest_actif, didnaindetecteur, dxpos, dypos, dmonde, didobj);
CREATE INDEX \"ObjsIdo\" ON public.objrins USING hash (ido);
CREATE INDEX avoir_nom_id_idx ON public.avoir_nom USING btree (id) INCLUDE (date_nom);
CREATE INDEX planet_osm_polygon_way_idx ON public.planet_osm_polygon USING gist (way) WITH (fillfactor='100');
CREATE INDEX trgm_idx_synonyms_complets_m1 ON public.synonyms_complets USING gin (m1 public.gin_trgm_ops);

";;



(*
type t =
  | String of string
  | Number of string (* float is not appropriate for decoding 64bit int *)
  | Object of obj
  | Array of t list
  | Bool of bool
  | Null
and obj = (string * t) list

 *)

(*
*)



let () =
  let statement = "SELECT user, email FROM users WHERE id = 7" in
  let ast = (Pg_query.raw_parse sql_index).parse_tree  in
  (*let _ = BatString.nreplace ~str:ast  ~sub:"\\" ~by:""  |> print_endline in
  let _ = print_endline ast in*)
  let json = BatString.nreplace ~str:ast  ~sub:"\\" ~by:""  |> Tiny_json.Json.parse  in
  let _ = validJsonOfJsont json |> print_endline in 
  let _ = json2Grammar json in
  ()
