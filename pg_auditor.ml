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


let sql3 = "	With counts as (
		Select count(*) as lancers, phom, pfem,nb from tirages_planetesproc where taille_echantillon = 24925 and source = 'mariages44' group by 2,3,4 order by 2,3 desc
	), minmax as (
		Select count(*) as total, min(nb), trunc(max(lancers),1) as milieuH, max(nb) , phom, pfem from counts Group by phom, pfem
	), tailleHisto as (
		Select sum(lancers) som, phom, pfem from counts group by phom, pfem -- toutes les sommes sont les mêmes, puisque c'est le nombre de tours de 
		--la boucle du bloc anonyme plus haut
	) -- Faut trouver le nb où on a 4360/2 lancers d'un côté et de l'autre
	--Ensuite, pour chaque ligne, on fait une somme des n premiers
	,datafin as (
		Select som, (100.0/som)*lancers , lancers
		, sum(lancers) over (partition by (t.phom, t.pfem) order by nb) as sumfcmilieu, nb, t.phom, t.pfem 
		From tailleHisto t inner join counts c on c.phom = t.phom and c.pfem = t.pfem
		--	Inner join 
		Order by phom, pfem, nb
	)
	Select 'mariages44' as nom_src, 24925 as taille_echantillon , pc, lancers
		,  Case When sumfcmilieu > som/2 then
			- ((100.0/som)*(sumfcmilieu-som)) --permet de savoir si on est à droite ou à gauche de la courbe gaussiene
		   Else -(100.0/som)*sumfcmilieu End as proba
		   , nb, (avg(nb) over (partition by (phom, pfem)))::int as nbmil
		   , phom, pfem
	From datafin
        Order by phom, pfem, nb;";;


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


let sql2Json  filename =
        let contenu = file2string filename in
        (*let _ = print_endline contenu in*)
         let ast = (Pg_query.raw_parse contenu).parse_tree  in
        (* let _ = print_endline ast in*)
         let json = BatString.nreplace ~str:ast  ~sub:"\\" ~by:""  |> Tiny_json.Json.parse  in
          validJsonOfJsont json |> print_endline 


let () =
        sql2Json Sys.argv.(1)
 (* let statement = "SELECT user, email FROM users WHERE id = 7" in
  let ast = (Pg_query.raw_parse sql3).parse_tree  in
  (*let _ = BatString.nreplace ~str:ast  ~sub:"\\" ~by:""  |> print_endline in
  let _ = print_endline ast in*)
  let json = BatString.nreplace ~str:ast  ~sub:"\\" ~by:""  |> Tiny_json.Json.parse  in
  let _ = validJsonOfJsont json |> print_endline in 
  let _ = json2Grammar json in
  ()*)
