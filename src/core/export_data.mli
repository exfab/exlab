(** Data export operations for translating core types to CSV format. *)

val escape_csv_field : string -> string
val plate_wells_to_csv_rows : (string * string option) list -> string

val plate_wells_to_numeric_csv :
  Types.plate_format -> (string * string option) list -> string

val plate_wells_to_matrix_csv :
  Types.plate_format -> (string * string option) list -> string

val samples_to_csv : Types.sample list -> string
val projects_to_csv : Types.project list -> string
val plates_to_csv : Types.plate list -> string
val products_to_csv : Types.product list -> string
val result_definitions_to_csv : Types.result_definition list -> string
val strains_to_csv : Types.strain list -> string

val plate_results_to_csv :
  definitions:Types.result_definition list -> Types.result_value list -> string

val generate_matrix :
  definitions:Types.result_definition list ->
  samples:Types.sample list ->
  plates:Types.plate list ->
  Types.result_value list ->
  Types.json

val generate_longitudinal_matrix :
  definitions:Types.result_definition list ->
  samples:Types.sample list ->
  plates:Types.plate list ->
  wells:Types.well list ->
  strains:Types.strain list ->
  Types.result_value list ->
  Types.json

val matrix_json_to_csv : Types.json -> string

type transfer_map_row = {
  source_sample_short_id : string;
  source_plate_name : string;
  source_well : string;
  dest_plate_name : string;
  dest_well : string;
  dest_sample_short_id : string;
}

val generate_transfer_map_csv : transfer_map_row list -> string
