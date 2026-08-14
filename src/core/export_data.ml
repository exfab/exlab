(** Data export operations for translating core types to CSV format. *)

open Types

(** [escape_csv_field s] escapes a string for CSV format. Example: 'a "b" c' ->
    '"a ""b"" c"' *)
let escape_csv_field s =
  if String.contains s '"' || String.contains s ',' || String.contains s '\n'
  then (
    let b = Buffer.create (String.length s + 2) in
    Buffer.add_char b '"';
    String.iter
      (fun c ->
        if c = '"' then Buffer.add_string b "\"\"" else Buffer.add_char b c)
      s;
    Buffer.add_char b '"';
    Buffer.contents b)
  else s

(** [plate_wells_to_csv_rows rows] converts a list of [coordinate, short_id]
    tuples into a basic CSV string. *)
let plate_wells_to_csv_rows rows =
  let buffer = Buffer.create 1024 in

  (* Header *)
  Buffer.add_string buffer "Coordinate,Short ID\n";

  (* Iterate and write rows to the buffer *)
  List.iter
    (fun (coordinate, sample_id_opt) ->
      let sid = Option.value ~default:"" sample_id_opt in

      (* Write the coordinate *)
      Buffer.add_string buffer coordinate;
      Buffer.add_char buffer ',';

      (* Write the Short ID *)
      Buffer.add_string buffer (escape_csv_field sid);

      (* Write newline *)
      Buffer.add_char buffer '\n')
    rows;
  Buffer.contents buffer

(** [samples_to_csv samples] serializes a list of [sample] records to a CSV
    string. *)
let samples_to_csv samples =
  let buffer = Buffer.create 1024 in

  (* Header *)
  Buffer.add_string buffer "ID,Short ID,Sample Type,Status\n";

  (* Iterate and write rows to the buffer *)
  List.iter
    (fun (sample : sample) ->
      (* Write Sample ID *)
      Buffer.add_string buffer (string_of_int sample.id);
      Buffer.add_char buffer ',';

      (* Write Short ID *)
      Buffer.add_string buffer (escape_csv_field sample.short_id);
      Buffer.add_char buffer ',';

      (* Write Sample Type *)
      Buffer.add_string buffer (escape_csv_field sample.sample_type);
      Buffer.add_char buffer ',';

      (* Write Status *)
      Buffer.add_string buffer
        (escape_csv_field (string_of_entity_status sample.status));

      (* Write newline *)
      Buffer.add_char buffer '\n')
    samples;
  Buffer.contents buffer

module StringSet = Set.Make (String)

(** [projects_to_csv projects] serializes a list of [project] records to a CSV
    string. *)
let projects_to_csv projects =
  let buffer = Buffer.create 1024 in

  (* 1. Discovery Pass: Find all unique metadata keys *)
  let metadata_keys =
    List.fold_left
      (fun acc (project : project) ->
        match project.metadata with
        | Some (`Assoc fields) ->
            List.fold_left (fun s (k, _) -> StringSet.add k s) acc fields
        | _ -> acc)
      StringSet.empty projects
  in
  let sorted_keys = StringSet.elements metadata_keys in

  (* Header *)
  Buffer.add_string buffer "ID,Short ID,Name,Status,Owner";
  List.iter
    (fun key ->
      Buffer.add_char buffer ',';
      Buffer.add_string buffer (escape_csv_field key))
    sorted_keys;
  Buffer.add_char buffer '\n';

  (* Data Pass *)
  List.iter
    (fun (project : project) ->
      (* Write Project ID *)
      Buffer.add_string buffer (string_of_int project.id);
      Buffer.add_char buffer ',';

      (* Write Short ID *)
      Buffer.add_string buffer (escape_csv_field project.short_id);
      Buffer.add_char buffer ',';

      (* Write Project Name *)
      Buffer.add_string buffer (escape_csv_field project.name);
      Buffer.add_char buffer ',';

      (* Write Project Status *)
      Buffer.add_string buffer
        (escape_csv_field (string_of_entity_status project.status));
      Buffer.add_char buffer ',';

      (* Write Project Owner *)
      Buffer.add_string buffer
        (escape_csv_field (Option.value ~default:"" project.owner));

      (* Write Metadata values *)
      let meta_assoc =
        match project.metadata with Some (`Assoc fields) -> fields | _ -> []
      in
      List.iter
        (fun key ->
          Buffer.add_char buffer ',';
          let val_str =
            match List.assoc_opt key meta_assoc with
            | Some (`String s) -> s
            | Some (`Int i) -> string_of_int i
            | Some (`Float f) -> string_of_float f
            | Some (`Bool b) -> string_of_bool b
            | Some other_json -> Yojson.Safe.to_string other_json
            | None -> ""
          in
          Buffer.add_string buffer (escape_csv_field val_str))
        sorted_keys;

      (* Write newline *)
      Buffer.add_char buffer '\n')
    projects;
  Buffer.contents buffer

(** [plates_to_csv plates] serializes a list of [plate] records to a CSV string.
*)
let plates_to_csv plates =
  let buffer = Buffer.create 1024 in

  (* Header *)
  Buffer.add_string buffer "ID,Short ID,Name,Format,Project ID\n";

  List.iter
    (fun (plate : plate) ->
      (* Write Plate ID *)
      Buffer.add_string buffer (string_of_int plate.id);
      Buffer.add_char buffer ',';

      (* Write Short ID *)
      Buffer.add_string buffer (escape_csv_field plate.short_id);
      Buffer.add_char buffer ',';

      (* Write Plate Name *)
      Buffer.add_string buffer (escape_csv_field plate.name);
      Buffer.add_char buffer ',';

      (* Write Plate Format *)
      Buffer.add_string buffer
        (escape_csv_field (string_of_plate_format plate.plate_format));
      Buffer.add_char buffer ',';

      (* Write Project ID *)
      Buffer.add_string buffer (string_of_int plate.project_id);

      (* Write newline *)
      Buffer.add_char buffer '\n')
    plates;
  Buffer.contents buffer

(** [products_to_csv products] serializes a list of [product] records to a CSV
    string. *)
let products_to_csv products =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "ID,Short ID,Name,Brand,Part Number,Created At\n";
  List.iter
    (fun (product : product) ->
      (* Write Product ID *)
      Buffer.add_string buffer (string_of_int product.id);
      Buffer.add_char buffer ',';

      (* Write Product Short ID *)
      Buffer.add_string buffer (escape_csv_field product.short_id);
      Buffer.add_char buffer ',';

      (* Write Product Name *)
      Buffer.add_string buffer (escape_csv_field product.name);
      Buffer.add_char buffer ',';

      (* Write Brand *)
      Buffer.add_string buffer
        (escape_csv_field (Option.value ~default:"" product.brand));
      Buffer.add_char buffer ',';

      (* Write Mfg part number *)
      Buffer.add_string buffer
        (escape_csv_field
           (Option.value ~default:"" product.manufacturer_part_number));
      Buffer.add_char buffer ',';

      (* Write Created At *)
      Buffer.add_string buffer
        (escape_csv_field (Float.to_string product.created_at));

      (* Write newline *)
      Buffer.add_char buffer '\n')
    products;
  Buffer.contents buffer

(** [result_definitions_to_csv result_definitions] serializes a list of
    [result_definition] records to a CSV string. *)
let result_definitions_to_csv result_definitions =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer
    "ID,Short ID,Name,Category,Required,Data Type,Unit,Created At\n";
  List.iter
    (fun (rd : result_definition) ->
      Buffer.add_string buffer (string_of_int rd.id);
      Buffer.add_char buffer ',';
      Buffer.add_string buffer (escape_csv_field rd.short_id);
      Buffer.add_char buffer ',';
      Buffer.add_string buffer (escape_csv_field rd.name);
      Buffer.add_char buffer ',';
      let category_name =
        match rd.category with Some c -> c.name | None -> "No Category"
      in
      Buffer.add_string buffer (escape_csv_field category_name);
      Buffer.add_char buffer ',';
      Buffer.add_string buffer (if rd.is_required then "True" else "False");
      Buffer.add_char buffer ',';
      Buffer.add_string buffer
        (escape_csv_field (ResultType.to_string rd.data_type));
      Buffer.add_char buffer ',';
      Buffer.add_string buffer
        (escape_csv_field (Option.value ~default:"" rd.unit));
      Buffer.add_char buffer ',';
      Buffer.add_string buffer
        (escape_csv_field (Float.to_string rd.created_at));
      Buffer.add_char buffer '\n')
    result_definitions;
  Buffer.contents buffer

(** [plate_wells_to_numeric_csv plate_format rows] converts a list of wells to a
    CSV using a 1-based numeric index instead of alpha-numeric coordinates. *)
let plate_wells_to_numeric_csv plate_format rows =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "well,sample_short_id\n";
  List.iter
    (fun (coordinate, sample_id_opt) ->
      match Plate.index_of_coordinate plate_format coordinate with
      | Ok index ->
          Buffer.add_string buffer (string_of_int index);
          Buffer.add_char buffer ',';
          Buffer.add_string buffer
            (escape_csv_field (Option.value ~default:"" sample_id_opt));
          Buffer.add_char buffer '\n'
      | Error _ -> () (* Skip invalid coordinates *))
    rows;
  Buffer.contents buffer

(** [plate_wells_to_matrix_csv plate_format rows] exports well assignments
    formatted as a 2D matrix (grid) representing the physical plate. *)
let plate_wells_to_matrix_csv plate_format rows =
  let plate_rows, plate_cols = Plate.get_plate_dimensions plate_format in
  let data_map = Hashtbl.create (List.length rows) in
  List.iter
    (fun (coordinate, sample_id_opt) ->
      Hashtbl.add data_map coordinate (Option.value ~default:"" sample_id_opt))
    rows;

  let buffer = Buffer.create 1024 in

  (* Header row *)
  Buffer.add_string buffer ",";
  for i = 1 to plate_cols do
    Buffer.add_string buffer (string_of_int i);
    if i < plate_cols then Buffer.add_char buffer ','
  done;
  Buffer.add_char buffer '\n';

  (* Data rows *)
  for i = 0 to plate_rows - 1 do
    let row_char = String.make 1 (char_of_int (int_of_char 'A' + i)) in
    Buffer.add_string buffer row_char;
    Buffer.add_char buffer ',';
    for j = 1 to plate_cols do
      let coord = row_char ^ string_of_int j in
      let value = try Hashtbl.find data_map coord with Not_found -> "" in
      Buffer.add_string buffer (escape_csv_field value);
      if j < plate_cols then Buffer.add_char buffer ','
    done;
    Buffer.add_char buffer '\n'
  done;

  Buffer.contents buffer

let strains_to_csv strains =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer
    "id,uid,genus,species,strain_name,genotype,parent_strain_id,notes,created_at,updated_at\n";
  List.iter
    (fun (s : strain) ->
      let id = string_of_int s.id in
      let uid = s.uid in
      let genus = escape_csv_field s.genus in
      let species = escape_csv_field s.species in
      let name = escape_csv_field s.strain_name in
      let genotype = escape_csv_field (Option.value s.genotype ~default:"") in
      let parent =
        match s.parent_strain_id with
        | Some pid -> string_of_int pid
        | None -> ""
      in
      let notes = escape_csv_field (Option.value s.notes ~default:"") in
      let created = Printf.sprintf "%f" s.created_at in
      let updated = Printf.sprintf "%f" s.updated_at in
      Printf.bprintf buffer "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n" id uid genus
        species name genotype parent notes created updated)
    strains;
  Buffer.contents buffer

(** [plate_results_to_csv results] serializes a list of plate results into CSV.
*)
let plate_results_to_csv ~definitions values =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "Result Type,Value,Updated At\n";

  let def_map = Hashtbl.create (List.length definitions) in
  List.iter
    (fun (d : result_definition) -> Hashtbl.add def_map d.id d.name)
    definitions;

  List.iter
    (fun (r : result_value) ->
      let def_name =
        try Hashtbl.find def_map r.result_definition_id
        with Not_found -> "Unknown"
      in
      let value_str =
        match r.value with
        | Some p -> (
            match p with
            | ResultPayload.String s -> s
            | ResultPayload.Integer i -> string_of_int i
            | ResultPayload.Float f -> string_of_float f
            | ResultPayload.Boolean b -> string_of_bool b
            | ResultPayload.Datetime f -> string_of_float f
            | ResultPayload.Date f -> string_of_float f
            | ResultPayload.FileLink s -> s
            | ( ResultPayload.StringSeries _ | ResultPayload.IntegerSeries _
              | ResultPayload.FloatSeries _ | ResultPayload.BooleanSeries _
              | ResultPayload.DatetimeSeries _ | ResultPayload.DateSeries _
              | ResultPayload.FileLinkSeries _ ) as ts ->
                Yojson.Safe.to_string (ResultPayload.yojson_of_t ts))
        | None -> ""
      in
      let updated_str = Printf.sprintf "%f" r.updated_at in
      Printf.bprintf buffer "%s,%s,%s\n"
        (escape_csv_field def_name)
        (escape_csv_field value_str)
        updated_str)
    values;
  Buffer.contents buffer

let matrix_json_to_csv matrix_json =
  let buffer = Buffer.create 1024 in
  match matrix_json with
  | `Assoc fields ->
      let columns =
        match List.assoc_opt "columns" fields with
        | Some (`List cols) -> cols
        | _ -> []
      in
      let data =
        match List.assoc_opt "data" fields with
        | Some (`List rows) -> rows
        | _ -> []
      in

      let col_keys =
        List.filter_map
          (function
            | `Assoc col_fields -> (
                match
                  ( List.assoc_opt "title" col_fields,
                    List.assoc_opt "data" col_fields )
                with
                | Some (`String title), Some (`String key) -> Some (title, key)
                | _ -> None)
            | _ -> None)
          columns
      in

      (* Write Header *)
      List.iteri
        (fun i (title, _) ->
          if i > 0 then Buffer.add_char buffer ',';
          Buffer.add_string buffer (escape_csv_field title))
        col_keys;
      Buffer.add_char buffer '\n';

      (* Write Data *)
      List.iter
        (function
          | `Assoc row_fields ->
              List.iteri
                (fun i (_, key) ->
                  if i > 0 then Buffer.add_char buffer ',';
                  let val_str =
                    match List.assoc_opt key row_fields with
                    | Some (`String s) -> s
                    | Some (`Int i) -> string_of_int i
                    | Some (`Float f) -> string_of_float f
                    | Some (`Bool b) -> string_of_bool b
                    | _ -> ""
                  in
                  Buffer.add_string buffer (escape_csv_field val_str))
                col_keys;
              Buffer.add_char buffer '\n'
          | _ -> ())
        data;
      Buffer.contents buffer
  | _ -> ""

let json_of_scalar_payload = function
  | ResultPayload.String s -> `String s
  | ResultPayload.Integer i -> `Int i
  | ResultPayload.Float f -> `Float f
  | ResultPayload.Boolean b -> `Bool b
  | ResultPayload.Datetime f -> `Float f
  | ResultPayload.Date f -> `Float f
  | ResultPayload.FileLink s -> `String s
  | _ -> `Null

let extract_time_series_data r payload time_points series_data scalars =
  let def_key = Printf.sprintf "def_%d" r.result_definition_id in
  let add_pts pts to_json =
    List.iter
      (fun (t, v) ->
        Hashtbl.replace time_points t true;
        let existing = try Hashtbl.find series_data t with Not_found -> [] in
        Hashtbl.replace series_data t ((def_key, to_json v) :: existing))
      pts
  in
  match payload with
  | ResultPayload.StringSeries pts -> add_pts pts (fun v -> `String v)
  | ResultPayload.IntegerSeries pts -> add_pts pts (fun v -> `Int v)
  | ResultPayload.FloatSeries pts -> add_pts pts (fun v -> `Float v)
  | ResultPayload.BooleanSeries pts -> add_pts pts (fun v -> `Bool v)
  | ResultPayload.DatetimeSeries pts -> add_pts pts (fun v -> `Float v)
  | ResultPayload.DateSeries pts -> add_pts pts (fun v -> `Float v)
  | ResultPayload.FileLinkSeries pts -> add_pts pts (fun v -> `String v)
  | scalar -> scalars := (def_key, json_of_scalar_payload scalar) :: !scalars

let group_values_by_entity values =
  let sample_results = Hashtbl.create 10 in
  let plate_results = Hashtbl.create 10 in
  List.iter
    (fun (r : result_value) ->
      match (r.sample_id, r.plate_id) with
      | Some sid, _ ->
          let existing =
            try Hashtbl.find sample_results sid with Not_found -> []
          in
          Hashtbl.replace sample_results sid (r :: existing)
      | None, Some pid ->
          let existing =
            try Hashtbl.find plate_results pid with Not_found -> []
          in
          Hashtbl.replace plate_results pid (r :: existing)
      | None, None -> ())
    values;
  (sample_results, plate_results)

let generate_matrix ~definitions ~samples ~plates values =
  (* 1. Analyze values to determine max time points for series *)
  let max_times = Hashtbl.create 10 in
  let used_defs = Hashtbl.create 10 in

  List.iter
    (fun (r : result_value) ->
      Hashtbl.replace used_defs r.result_definition_id true;
      match r.value with
      | None -> ()
      | Some payload ->
          let max_t =
            match payload with
            | ResultPayload.StringSeries pts ->
                List.fold_left (fun acc (t, _) -> max acc t) 0 pts
            | ResultPayload.IntegerSeries pts ->
                List.fold_left (fun acc (t, _) -> max acc t) 0 pts
            | ResultPayload.FloatSeries pts ->
                List.fold_left (fun acc (t, _) -> max acc t) 0 pts
            | ResultPayload.BooleanSeries pts ->
                List.fold_left (fun acc (t, _) -> max acc t) 0 pts
            | ResultPayload.DatetimeSeries pts ->
                List.fold_left (fun acc (t, _) -> max acc t) 0 pts
            | ResultPayload.DateSeries pts ->
                List.fold_left (fun acc (t, _) -> max acc t) 0 pts
            | ResultPayload.FileLinkSeries pts ->
                List.fold_left (fun acc (t, _) -> max acc t) 0 pts
            | _ -> 0
          in
          if max_t > 0 then begin
            let current_max =
              try Hashtbl.find max_times r.result_definition_id
              with Not_found -> 0
            in
            Hashtbl.replace max_times r.result_definition_id
              (max current_max max_t)
          end)
    values;

  (* 2. Build Columns Schema *)
  let def_map = Hashtbl.create (List.length definitions) in
  List.iter
    (fun (d : result_definition) -> Hashtbl.add def_map d.id d)
    definitions;

  let base_columns =
    [
      `Assoc
        [
          ("title", `String "Entity Type");
          ("data", `String "entity_type");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Short ID");
          ("data", `String "short_id");
          ("visible", `Bool true);
        ];
    ]
  in

  let result_columns =
    List.filter_map
      (fun (d : result_definition) ->
        if not (Hashtbl.mem used_defs d.id) then None
        else
          let max_t = try Hashtbl.find max_times d.id with Not_found -> 0 in
          if max_t = 0 then
            Some
              [
                `Assoc
                  [
                    ("title", `String d.name);
                    ("data", `String (Printf.sprintf "def_%d_scalar" d.id));
                    ("visible", `Bool true);
                  ];
              ]
          else
            let cols = ref [] in
            for t = 1 to max_t do
              cols :=
                `Assoc
                  [
                    ("title", `String (Printf.sprintf "%s (Pt %d)" d.name t));
                    ("data", `String (Printf.sprintf "def_%d_pt_%d" d.id t));
                    ("visible", `Bool false);
                  ]
                :: !cols
            done;
            Some (List.rev !cols))
      definitions
    |> List.flatten
  in

  let columns = base_columns @ result_columns in

  (* 3. Group values by entity *)
  let sample_results, plate_results = group_values_by_entity values in

  let build_row entity_type id short_id entity_results =
    let base_fields =
      [
        ("id", `Int id);
        ("entity_type", `String entity_type);
        ("short_id", `String short_id);
      ]
    in

    let result_fields =
      List.map
        (fun (r : result_value) ->
          match r.value with
          | None -> []
          | Some payload -> (
              let get_json_val = json_of_scalar_payload in
              match payload with
              | ResultPayload.StringSeries pts ->
                  List.map
                    (fun (t, v) ->
                      ( Printf.sprintf "def_%d_pt_%d" r.result_definition_id t,
                        `String v ))
                    pts
              | ResultPayload.IntegerSeries pts ->
                  List.map
                    (fun (t, v) ->
                      ( Printf.sprintf "def_%d_pt_%d" r.result_definition_id t,
                        `Int v ))
                    pts
              | ResultPayload.FloatSeries pts ->
                  List.map
                    (fun (t, v) ->
                      ( Printf.sprintf "def_%d_pt_%d" r.result_definition_id t,
                        `Float v ))
                    pts
              | ResultPayload.BooleanSeries pts ->
                  List.map
                    (fun (t, v) ->
                      ( Printf.sprintf "def_%d_pt_%d" r.result_definition_id t,
                        `Bool v ))
                    pts
              | ResultPayload.DatetimeSeries pts ->
                  List.map
                    (fun (t, v) ->
                      ( Printf.sprintf "def_%d_pt_%d" r.result_definition_id t,
                        `Float v ))
                    pts
              | ResultPayload.DateSeries pts ->
                  List.map
                    (fun (t, v) ->
                      ( Printf.sprintf "def_%d_pt_%d" r.result_definition_id t,
                        `Float v ))
                    pts
              | ResultPayload.FileLinkSeries pts ->
                  List.map
                    (fun (t, v) ->
                      ( Printf.sprintf "def_%d_pt_%d" r.result_definition_id t,
                        `String v ))
                    pts
              | scalar ->
                  [
                    ( Printf.sprintf "def_%d_scalar" r.result_definition_id,
                      get_json_val scalar );
                  ]))
        entity_results
      |> List.flatten
    in
    `Assoc (base_fields @ result_fields)
  in

  let sample_rows =
    List.map
      (fun (s : sample) ->
        let res = try Hashtbl.find sample_results s.id with Not_found -> [] in
        build_row "Sample" s.id s.short_id res)
      samples
  in

  let plate_rows =
    List.map
      (fun (p : plate) ->
        let res = try Hashtbl.find plate_results p.id with Not_found -> [] in
        build_row "Plate" p.id p.short_id res)
      plates
  in

  let data = sample_rows @ plate_rows in

  `Assoc [ ("columns", `List columns); ("data", `List data) ]

let build_indexes ~definitions ~samples ~plates ~wells ~strains =
  let strain_map = Hashtbl.create (List.length strains) in
  List.iter
    (fun (s : strain) -> Hashtbl.add strain_map s.id s.strain_name)
    strains;
  let sample_map = Hashtbl.create (List.length samples) in
  List.iter (fun (s : sample) -> Hashtbl.add sample_map s.id s) samples;
  let plate_map = Hashtbl.create (List.length plates) in
  List.iter (fun (p : plate) -> Hashtbl.add plate_map p.id p.short_id) plates;
  let sample_to_well = Hashtbl.create (List.length wells) in
  List.iter
    (fun (w : well) ->
      match w.sample_id with
      | Some sid -> Hashtbl.add sample_to_well sid w
      | None -> ())
    wells;
  let def_map = Hashtbl.create (List.length definitions) in
  List.iter
    (fun (d : result_definition) -> Hashtbl.add def_map d.id d)
    definitions;
  (strain_map, sample_map, plate_map, sample_to_well, def_map)

let generate_longitudinal_matrix ~definitions ~samples ~plates ~wells ~strains
    values =
  (* 1. Indexes *)
  let strain_map, sample_map, plate_map, sample_to_well, _def_map =
    build_indexes ~definitions ~samples ~plates ~wells ~strains
  in

  (* 2. Dynamic Columns *)
  let base_columns =
    [
      `Assoc
        [
          ("title", `String "Strain");
          ("data", `String "strain");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Source");
          ("data", `String "source");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Experimental");
          ("data", `String "experimental");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Plate");
          ("data", `String "plate");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Well_Pos");
          ("data", `String "well_pos");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Time_Point");
          ("data", `String "time_point");
          ("visible", `Bool true);
        ];
    ]
  in

  let used_defs = Hashtbl.create 10 in
  List.iter
    (fun (r : result_value) ->
      Hashtbl.replace used_defs r.result_definition_id true)
    values;

  let dynamic_columns =
    List.filter_map
      (fun (d : result_definition) ->
        if Hashtbl.mem used_defs d.id then
          Some
            (`Assoc
               [
                 ("title", `String d.name);
                 ("data", `String (Printf.sprintf "def_%d" d.id));
                 ("visible", `Bool true);
               ])
        else None)
      definitions
  in
  let columns = base_columns @ dynamic_columns in

  (* 3. Group values by sample and plate *)
  let sample_results, plate_results = group_values_by_entity values in

  (* 4. Row generation *)
  let rows =
    List.filter_map
      (fun (s : sample) ->
        if s.category <> Experimental || s.status = Archived then None
        else
          (* Resolve structural metadata *)
          let source_sample_id =
            match s.parent_sample_id with
            | Some pid -> (
                try Some (Hashtbl.find sample_map pid) with Not_found -> None)
            | None -> None
          in

          let strain_name =
            match source_sample_id with
            | Some src -> (
                match src.strain_id with
                | Some strain_id -> (
                    try Hashtbl.find strain_map strain_id with Not_found -> "")
                | None -> "")
            | None -> ""
          in
          let source_short_id =
            match source_sample_id with Some src -> src.short_id | None -> ""
          in
          let experimental_short_id = s.short_id in

          let plate_short_id, well_pos, plate_id_opt =
            try
              let w = Hashtbl.find sample_to_well s.id in
              let p_id =
                try Hashtbl.find plate_map w.plate_id with Not_found -> ""
              in
              (p_id, w.coordinate, Some w.plate_id)
            with Not_found -> ("", "", None)
          in

          let sample_res =
            try Hashtbl.find sample_results s.id with Not_found -> []
          in
          let plate_res =
            match plate_id_opt with
            | Some pid -> (
                try Hashtbl.find plate_results pid with Not_found -> [])
            | None -> []
          in
          let res = sample_res @ plate_res in

          (* Separate scalars from series and collect time points *)
          let scalars = ref [] in
          let series_data = Hashtbl.create 10 in
          (* time_point -> (def_id, json_value) list *)
          let time_points = Hashtbl.create 10 in

          List.iter
            (fun (r : result_value) ->
              match r.value with
              | None -> ()
              | Some payload ->
                  extract_time_series_data r payload time_points series_data
                    scalars)
            res;

          let unique_time_points =
            let pts = Hashtbl.fold (fun t _ acc -> t :: acc) time_points [] in
            if pts = [] then [ 1 ] else List.sort compare pts
          in

          let sample_rows =
            List.map
              (fun t ->
                let time_series_fields =
                  try Hashtbl.find series_data t with Not_found -> []
                in
                let dynamic_fields = !scalars @ time_series_fields in
                let sorted_dynamic_fields =
                  List.sort
                    (fun (k1, _) (k2, _) -> String.compare k1 k2)
                    dynamic_fields
                in
                let row_fields =
                  [
                    ("strain", `String strain_name);
                    ("source", `String source_short_id);
                    ("experimental", `String experimental_short_id);
                    ("plate", `String plate_short_id);
                    ("well_pos", `String well_pos);
                    ("time_point", `Int t);
                  ]
                  @ sorted_dynamic_fields
                in
                `Assoc row_fields)
              unique_time_points
          in

          Some sample_rows)
      samples
    |> List.flatten
  in

  `Assoc [ ("columns", `List columns); ("data", `List rows) ]

type transfer_map_row = {
  source_sample_short_id : string;
  source_plate_name : string;
  source_well : string;
  dest_plate_name : string;
  dest_well : string;
  dest_sample_short_id : string;
}

let generate_transfer_map_csv (rows : transfer_map_row list) =
  let headers = "Source_Sample_ID,Source_Plate_Name,Source_Well,Dest_Plate_Name,Dest_Well,Experimental_Sample_ID" in
  let data_rows =
    List.map
      (fun row ->
        Printf.sprintf "%s,%s,%s,%s,%s,%s"
          (escape_csv_field row.source_sample_short_id)
          (escape_csv_field row.source_plate_name)
          (escape_csv_field row.source_well)
          (escape_csv_field row.dest_plate_name)
          (escape_csv_field row.dest_well)
          (escape_csv_field row.dest_sample_short_id))
      rows
  in
  String.concat "\n" (headers :: data_rows)
