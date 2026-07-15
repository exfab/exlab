(** Pure algorithmic module for randomized plate generation. *)

type strategy = Simple | Neighbor_aware
type coordinate = int * int

type state = {
  used_edge_samples : string list;
  neighbor_pairs : (string * string) list;
}
(** Represents the state tracked across multiple plate generations *)

val generate_plate :
  strategy:strategy ->
  rows:int ->
  cols:int ->
  samples:string list ->
  fixed_map:(coordinate * string) list ->
  state:state ->
  (coordinate * string) list * state
(** Generates a randomized plate layout. [rows] and [cols] define the plate
    dimensions. [samples] is a list of source sample IDs (replicates should be
    duplicated in this list). [fixed_map] is an association list of pre-defined
    fixed positions, e.g., for controls. [state] is the accumulated state from
    previous plate generations. *)

val generate_multiple_plates :
  strategy:strategy ->
  rows:int ->
  cols:int ->
  samples_per_plate:string list list ->
  fixed_map:(coordinate * string) list ->
  (coordinate * string) list list
(** Convenience function to generate multiple plates sequentially, maintaining
    state across them. *)
