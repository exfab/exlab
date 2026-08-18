(** This module defines the Data Transfer Objects (DTOs) used for API request
    and response payloads. These types bridge the gap between the raw JSON sent
    by clients and the internal OCaml types used in the core logic and storage
    layers.

    Each submodule corresponds to a specific resource (e.g., Project, Sample)
    and defines the types for various operations like creation, updates, and
    list responses.

    The `[@@deriving yojson]` annotation automatically generates the necessary
    functions for converting these types to and from the Yojson format. *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(** {1 Common}
    Common and shared API types. *)
module Common = struct
  type status_response = { status : string } [@@deriving yojson]
  (** A standard response for operations that return a status message, like
      updates or deletions. *)
end

(** {1 Project}
    DTOs for Project-related operations. *)
module Project = struct
  (* The request payload for creating a new project. A `short_id` can be
      optionally provided; if not, the system will generate one. *)
  type create = {
    name : string;
    description : string option; [@yojson.option]
    contact_name : string option; [@yojson.option]
    owner : string option; [@yojson.option]
    status : Exlab_core.Types.entity_status option; [@yojson.option]
    short_id : string option; [@yojson.option]
    metadata : Exlab_core.Types.json option; [@yojson.option]
  }
  [@@deriving yojson]

  (* The request payload for updating an existing project. The `status` field
      is optional and will default to the existing status if not provided. *)
  type update = {
    name : string;
    description : string option; [@yojson.option]
    contact_name : string option; [@yojson.option]
    owner : string option; [@yojson.option]
    status : Exlab_core.Types.entity_status option; [@yojson.option]
    metadata : Exlab_core.Types.json option; [@yojson.option]
  }
  [@@deriving yojson]

  (* The response payload for a list of projects, including the total count. *)
  type list_response = { data : Exlab_core.Types.project list; count : int }
  [@@deriving yojson]

  type project_user_request = { email : string } [@@deriving yojson]
end

(** {1 Sample}
    DTOs for Sample-related operations. *)
module Sample = struct
  (* The request payload for creating a new sample. Includes fields for linking
      to a parent sample and associating result definitions. *)
  type create = {
    sample_type : string;
    category : string option; [@yojson.option]
    parent_sample_id : int option; [@yojson.option]
    parent_sample_short_id : string option; [@yojson.option]
    strain_id : int option; [@yojson.option]
    community_id : int option; [@yojson.option]
    result_definition_ids : int list; [@default []]
    genus : string option; [@yojson.option]
    species : string option; [@yojson.option]
    strain_name : string option; [@yojson.option]
    genotype : string option; [@yojson.option]
  }
  [@@deriving yojson, show, eq]

  (* The request payload for updating an existing sample. *)
  type update = {
    sample_type : string;
    category : string option;
    parent_sample_id : int option;
    strain_id : int option;
    community_id : int option;
    status : Exlab_core.Types.entity_status option; [@yojson.option]
  }
  [@@deriving yojson]

  (* The response payload for a list of samples. *)
  type list_response = { data : Exlab_core.Types.sample list; count : int }
  [@@deriving yojson]

  (* A detailed view of a sample, including its associated project and
      a list of its result values. This is typically used for a sample's
      "details" page. *)
  type detailed = {
    sample : Exlab_core.Types.sample;
    project : Exlab_core.Types.project;
    results : Exlab_core.Types.result_value list;
  }
  [@@deriving yojson]

  (* Represents the location of a sample in a well on a plate. *)
  type location = { plate_id : int; well_coordinate : string }
  [@@deriving yojson]

  (* The response payload for a list of a sample's locations. *)
  type location_list_response = { data : location list; count : int }
  [@@deriving yojson]

  (* The wrapper for a bulk sample creation request, containing a list of
      sample items to be created. *)
  type bulk_create_request = { samples : create list } [@@deriving yojson]

  type bulk_create_summary = {
    created_samples : int;
    linked_strains : int;
    created_strains : int;
  }
  [@@deriving yojson]

  type bulk_create_response = {
    samples : Exlab_core.Types.sample list;
    summary : bulk_create_summary;
  }
  [@@deriving yojson]
end

(** {1 Plate}
    DTOs for Plate-related operations. *)
module Plate = struct
  (* The request payload for creating a new plate. A `product_id` can be
      optionally provided to link the plate to a specific labware product. *)
  type create = {
    name : string;
    plate_format : Exlab_core.Types.plate_format;
    project_id : int;
    product_id : int option; [@yojson.option]
    category : string option; [@yojson.option]
  }
  [@@deriving yojson]

  (* The response payload for a list of plates. *)
  type list_response = { data : Exlab_core.Types.plate list; count : int }
  [@@deriving yojson]

  type auto_fill_request = {
    wells : string list;
    sample_short_ids : string list;
    strategy : string option; [@yojson.option]
  }
  [@@deriving yojson]

  type bulk_unassign_request = { wells : string list } [@@deriving yojson]

  type plate_plan_request = {
    source_sample_short_ids : string list;
    replicates : int;
    plate_name_prefix : string;
    plate_format : string;
    reserved_wells : string list;
    strategy : string; [@default "simple"]
    num_plates : int; [@default 1]
    num_blanks : int; [@default 0]
  }
  [@@deriving yojson]

  type transfer_map_request = {
    source_plate_id : int;
    destination_plate_ids : int list;
  }
  [@@deriving yojson]

  type well_response = { well : Exlab_core.Types.well; is_edge : bool }
  [@@deriving yojson]

  (* A detailed view of a plate, including the plate itself and a list of all
      its wells. *)
  type detailed = { plate : Exlab_core.Types.plate; wells : well_response list }
  [@@deriving yojson]

  type well_layout_item = { well : string; sample_short_id : string }
  [@@deriving yojson, show, eq]

  type well_layout = well_layout_item list [@@deriving yojson]

  type bulk_action_item =
    | Create_blank of { plate_name : string }
    | Create_with_layout of {
        plate_name : string;
        well : string;
        sample_short_id : string;
      }
  [@@deriving yojson, show, eq]

  type bulk_layout_item = {
    plate_name : string;
    well : string;
    sample_short_id : string;
  }
  [@@deriving yojson, show, eq]

  (* A list of all supported plate formats. *)
  let all_formats =
    [
      ( Exlab_core.Types.Well_24,
        Exlab_core.Types.Well_48,
        Exlab_core.Types.Well_96,
        Exlab_core.Types.Well_384 );
    ]
end

(** {1 Well}
    DTOs for Well-related operations. *)
module Well = struct
  (* The request payload for updating a well, typically to assign a sample. *)
  type update = { sample_id : int option } [@@deriving yojson]
end

(** {1 ResultCategory}
    DTOs for Result Category operations. *)
module ResultCategory = struct
  (* The request payload for creating a new result category. *)
  type create = { name : string; description : string option [@yojson.option] }
  [@@deriving yojson]

  (* The request payload for updating an existing result category. *)
  type update = { name : string; description : string option [@yojson.option] }
  [@@deriving yojson]

  (* The response payload for a list of result categories. *)
  type list_response = {
    data : Exlab_core.Types.result_category list;
    count : int;
  }
  [@@deriving yojson]
end

(** {1 ResultDefinition}
    DTOs for Result Definition operations. *)
module ResultDefinition = struct
  (* The request payload for creating a new result definition. This defines
      the schema for a type of result that can be recorded. *)
  type create = {
    short_id : string;
    name : string;
    description : string option; [@yojson.option]
    data_type : Exlab_core.Types.ResultType.t;
    unit : string option; [@yojson.option]
    category_id : int option; [@yojson.option]
    is_required : bool; [@default false]
  }
  [@@deriving yojson]

  (* The request payload for updating an existing result definition. *)
  type update = {
    short_id : string;
    name : string;
    description : string option; [@yojson.option]
    data_type : Exlab_core.Types.ResultType.t;
    unit : string option; [@yojson.option]
    category_id : int option; [@yojson.option]
    is_required : bool;
  }
  [@@deriving yojson]

  (* The response payload for a list of result definitions. *)
  type list_response = {
    data : Exlab_core.Types.result_definition list;
    count : int;
  }
  [@@deriving yojson]
end

(** {1 ResultValue}
    DTOs for Result Value operations. *)
module ResultValue = struct
  (* The request payload for creating a new result value. This represents a
      specific measurement or observation. *)
  type create = {
    result_definition_id : int;
    value : Exlab_core.Types.ResultPayload.t option; [@yojson.option]
  }
  [@@deriving yojson]

  (* The request payload for updating an existing result value completely (PUT). *)
  type update = {
    value : Exlab_core.Types.ResultPayload.t option; [@yojson.option]
  }
  [@@deriving yojson]

  (* The request payload for appending a single data point to a TimeSeries (PATCH). *)
  type patch_append = {
    time : int option; [@yojson.option]
    value : Exlab_core.Types.ResultPayload.t;
  }
  [@@deriving yojson]

  (* The response payload for a list of result values. *)
  type list_response = {
    data : Exlab_core.Types.result_value list;
    count : int;
  }
  [@@deriving yojson]
end

(** {1 Product}
    DTOs for Product-related operations. *)
module Product = struct
  (* The request payload for creating a new product (e.g., a type of labware
      or reagent). *)
  type create = {
    name : string;
    brand : string option; [@yojson.option]
    manufacturer_part_number : string option; [@yojson.option]
    description : string option; [@yojson.option]
  }
  [@@deriving yojson, show, eq]

  (* The request payload for updating an existing product. *)
  type update = {
    name : string;
    brand : string option; [@yojson.option]
    manufacturer_part_number : string option; [@yojson.option]
    description : string option; [@yojson.option]
  }
  [@@deriving yojson, show, eq]

  (* The response payload for a list of products. *)
  type list_response = { data : Exlab_core.Types.product list; count : int }
  [@@deriving yojson]

  (* The wrapper for a bulk product creation request, containing a list of
      product items to be created. *)
  type bulk_create_request = { products : create list } [@@deriving yojson]
end

(** {1 StrainLink}
    DTOs for linking a strain to an external database. *)
module StrainLink = struct
  (* The request payload for creating a new link between a strain and an
      external database. *)
  type create = {
    strain_id : int;
    external_db_definition_id : int;
    value : string;
  }
  [@@deriving yojson]

  (* The request payload for updating a strain-to-database link. *)
  type update = {
    strain_id : int;
    external_db_definition_id : int option; [@yojson.option]
    value : string option; [@yojson.option]
  }
  [@@deriving yojson]

  type detailed = {
    id : int;
    external_db_definition_id : int;
    db_name : string;
    value : string;
    resolved_url : string option;
  }
  [@@deriving yojson]

  (* The response payload for a list of strain-to-database links. *)
  type list_response = {
    data : Exlab_core.Types.strain_external_link list;
    count : int;
  }
  [@@deriving yojson]
end

(** {1 Strain}
    DTOs for Strain-related operations. *)
module Strain = struct
  (* Heler type for creation. *)
  type link_request = { external_db_definition_id : int; value : string }
  [@@deriving yojson, show, eq]

  (* The request payload for creating a new strain. *)
  type create = {
    genus : string;
    species : string;
    strain_name : string;
    genotype : string option; [@default None]
    parent_strain_id : int option; [@default None]
    notes : string option; [@default None]
    links : link_request list; [@default []]
  }
  [@@deriving yojson, show, eq]

  (* The request payload for updating an existing strain. *)
  type update = {
    genus : string;
    species : string;
    strain_name : string;
    genotype : string option; [@default None]
    parent_strain_id : int option; [@default None]
    notes : string option; [@default None]
  }
  [@@deriving yojson]

  (* The response payload for a list of strains. *)
  type list_response = { data : Exlab_core.Types.strain list; count : int }
  [@@deriving yojson]

  type source_sample_entry = {
    sample : Exlab_core.Types.sample;
    project : Exlab_core.Types.project;
  }
  [@@deriving yojson]

  (* A detailed view of a strain, including its associated external database links. *)
  type detailed = {
    strain : Exlab_core.Types.strain;
    external_links : StrainLink.detailed list;
    source_samples : source_sample_entry list;
  }
  [@@deriving yojson]

  type bulk_create_request = { strains : create list } [@@deriving yojson]
end

(** {1 ExternalDb}
    DTOs for External Database-related operations. *)
module ExternalDb = struct
  (* The request payload for creating a new external database definition. *)
  type create = { name : string; url_template : string } [@@deriving yojson]

  (* The request payload for updating an external database definition. *)
  type update = { name : string; url_template : string } [@@deriving yojson]

  type list_response = {
    data : Exlab_core.Types.external_db_definition list;
    count : int;
  }
  [@@deriving yojson]
  (* The response payload for a list of external database definitions. *)
end

(** {1 Setting}
    DTOs for System Setting operations. *)
module Setting = struct
  type update = {
    value : Exlab_core.Types.json;
    description : string option; [@yojson.option]
  }
  [@@deriving yojson]
end

(** {1 Identifier}
    A variant type to standardize the different kinds of identifiers that can be
    used to look up resources. This allows for flexibility in referencing
    resources via their integer ID, UUID, or a user-friendly Short ID. *)
module Identifier = struct
  type t = Id of int | Uuid of string | Short_id of string

  (** Parses a string to determine the type of identifier it represents. It
      first attempts to parse the string as an integer. If that fails, it checks
      if it's a valid UUID. If not, it defaults to a Short_id.

      @param str The input string to parse.
      @return The corresponding identifier variant. *)
  let of_string str =
    match int_of_string_opt str with
    | Some i -> Id i
    | None -> (
        match Uuidm.of_string str with
        | Some _ -> Uuid str
        | None -> Short_id str)

  (* Converts an identifier to its Yojson representation. *)
  let yojson_of_t = function
    | Id i -> `Int i
    | Uuid s -> `String s
    | Short_id s -> `String s

  (* Converts a Yojson value to an identifier. *)
  let t_of_yojson = function
    | `Int i -> Id i
    | `String s -> of_string s
    | json ->
        Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
          "Expected int or string for identifier" json
end

(** {1 User}
    DTOs for User-related operations. *)
module User = struct
  type create = {
    email : string;
    password : string;
    role : Exlab_core.Types.user_role; (* Using the variant directly! *)
  }
  [@@deriving yojson]
  (* The request payload for creating a new user. *)

  type update = {
    email : string;
    password : string option; [@yojson.option]
    role : Exlab_core.Types.user_role; (* Using the variant directly! *)
  }
  [@@deriving yojson]
  (* The request payload for updating an existing user. *)

  type safe_user = {
    id : int;
    uid : string;
    email : string;
    role : Exlab_core.Types.user_role;
    created_at : float;
    updated_at : float;
  }
  [@@deriving yojson]
  (* A safe representation of a user to send to the client, omitting password hashes. *)

  let to_safe_user (u : Exlab_core.Types.user) : safe_user =
    {
      id = u.id;
      uid = u.uid;
      email = u.email;
      role = u.role;
      created_at = u.created_at;
      updated_at = u.updated_at;
    }

  type list_response = { data : safe_user list; count : int }
  [@@deriving yojson]
  (* The response payload for a list of users. *)
end

(** {1 Community}
    DTOs for Community and Community Member operations. *)
module CommunityMember = struct
  (* The request payload for adding a member to a community. *)
  type create = {
    strain_id : int option; [@yojson.option]
    label : string;
    taxon : string option; [@yojson.option]
  }
  [@@deriving yojson, show, eq]

  (* The request payload for updating a community member. *)
  type update = {
    strain_id : int option; [@yojson.option]
    label : string;
    taxon : string option; [@yojson.option]
  }
  [@@deriving yojson]

  type bulk_create_request = { members : create list } [@@deriving yojson]
end

module Community = struct
  (* The request payload for creating a new community. *)
  type create = {
    name : string;
    notes : string option; [@yojson.option]
    metadata : Exlab_core.Types.json option; [@yojson.option]
  }
  [@@deriving yojson, show, eq]

  (* The request payload for updating an existing community. *)
  type update = {
    name : string;
    notes : string option; [@yojson.option]
    metadata : Exlab_core.Types.json option; [@yojson.option]
  }
  [@@deriving yojson]

  (* The response payload for a list of communities. *)
  type list_response = { data : Exlab_core.Types.community list; count : int }
  [@@deriving yojson]

  type source_sample_entry = {
    sample : Exlab_core.Types.sample;
    project : Exlab_core.Types.project;
  }
  [@@deriving yojson]

  (* A detailed view of a community, including its members and source samples. *)
  type detailed = {
    community : Exlab_core.Types.community;
    members : Exlab_core.Types.community_member list;
    source_samples : source_sample_entry list;
  }
  [@@deriving yojson]
end

(** {1 Dashboard}
    DTOs for dynamically generated DataTables dashboards. *)
module Dashboard = struct
  type column = {
    title : string;
    data : string;
    visible : bool; [@default true]
  }
  [@@deriving yojson, show, eq]

  type matrix_response = {
    columns : column list;
    data : Exlab_core.Types.json list;
  }
  [@@deriving yojson, show, eq]
end

(** {1 Auth}
    DTOs for Authentication-related operations. *)
module Auth = struct
  type login_request = { email : string; password : string } [@@deriving yojson]

  type password_update = { current_password : string; new_password : string }
  [@@deriving yojson]
end
