(** Plate distribution planning module *)

val distribute_round_robin :
  total_plates:int ->
  usable_wells:string list ->
  items:'a list ->
  ((string * 'a) list list, string) result
(** [distribute_round_robin ~total_plates ~usable_wells ~items] distributes a
    list of items across a given number of plates, using the provided list of
    usable wells for each plate. Returns a list of layouts (one layout per
    plate). Each layout is a list of (well_coordinate, item). If it runs out of
    usable wells across all plates before all items are assigned, it returns an
    Error. *)

val generate_autofill_layout :
  strategy:string option ->
  wells:string list ->
  sample_ids:string list ->
  ((string * string) list, string) result
(** [generate_autofill_layout ~strategy ~wells ~sample_ids] generates a layout
    mapping wells to sample IDs based on the specified strategy (e.g.
    "sequential"). *)

val generate_shuffled_layouts :
  strategy:Well_shuffled.strategy ->
  format:Types.plate_format ->
  reserved_wells:string list ->
  fixed_maps:(string * (string * 'a)) list list ->
  items_per_plate:(string * 'a) list list ->
  num_blanks:int ->
  ((string * 'a) list list, string) result
(** [generate_shuffled_layouts ~strategy ~format ~reserved_wells ~fixed_maps ~items_per_plate ~num_blanks]
    generates multiple randomized plate layouts using the Well_shuffled
    algorithmic engine. [strategy] is the shuffling strategy (Simple or
    Neighbor_aware). [format] is the plate dimensions (e.g. Plate_96).
    [reserved_wells] is a list of alphanumeric coordinates to leave completely blank.
    [fixed_maps] is a list of lists of fixed control wells per plate:
    [(well_coordinate, (source_id, item))]. [items_per_plate] is a list of lists
    of variable samples to shuffle per plate: [(source_id, item)]. 
    [num_blanks] is the number of free-floating blanks to add to each plate. *)
