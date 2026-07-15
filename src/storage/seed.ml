(** Database seeder for populating a fresh instance with comprehensive test
    data. *)

let run () =
  match Sys.getenv_opt "AUTO_POPULATE_TEST_DATA" with
  | Some "true" -> (
      let open Lwt.Syntax in
      print_endline
        "AUTO_POPULATE_TEST_DATA is true. Checking database state...";
      let* projects_result = Project.get_all () in
      match projects_result with
      | Ok [] -> (
          print_endline
            "Database is empty. Starting comprehensive population...";

          let seed_operation =
            let open Lwt_result.Syntax in
            (* --- 1. External DBs --- *)
            let* db_ncbi =
              External_db_definition.add ~name:"NCBI GenBank"
                ~url_template:"https://www.ncbi.nlm.nih.gov/nuccore/"
            in
            let* _db_uniprot =
              External_db_definition.add ~name:"UniProt"
                ~url_template:"https://www.uniprot.org/uniprotkb/"
            in

            (* --- 2. Categories & Definitions --- *)
            let* cat_growth =
              Result_category.add ~name:"Growth Metrics"
                ~description:(Some "Kinetics and density.")
            in
            let* cat_genomics =
              Result_category.add ~name:"Genomics"
                ~description:(Some "Sequencing and assembly.")
            in
            let* cat_metabolomics =
              Result_category.add ~name:"Metabolomics"
                ~description:(Some "Small molecule quantification.")
            in

            let* rd_od600 =
              Result_definition.add ~short_id:"OD600" ~name:"Optical Density"
                ~description:(Some "Standard OD at 600nm.")
                ~data_type:Exlab_core.Types.ResultType.Float ~unit:(Some "AU")
                ~category_id:cat_growth.id ()
            in

            let* _rd_grate =
              Result_definition.add ~short_id:"grate" ~name:"Growth Rate"
                ~description:(Some "Specific growth rate.")
                ~data_type:Exlab_core.Types.ResultType.Float ~unit:(Some "1/h")
                ~category_id:cat_growth.id ()
            in

            let* _rd_seq_qual =
              Result_definition.add ~short_id:"seqqual" ~name:"Sequence Quality"
                ~description:(Some "PHRED score or similar.")
                ~data_type:Exlab_core.Types.ResultType.String ~unit:None
                ~category_id:cat_genomics.id ()
            in

            let* rd_is_valid =
              Result_definition.add ~short_id:"is_valid" ~name:"Verified"
                ~description:(Some "Quality control pass/fail.")
                ~data_type:Exlab_core.Types.ResultType.Boolean ~unit:None
                ~category_id:cat_genomics.id ()
            in

            let* rd_conc =
              Result_definition.add ~short_id:"conc" ~name:"Concentration"
                ~description:(Some "Analyte concentration.")
                ~data_type:Exlab_core.Types.ResultType.Float ~unit:(Some "mg/L")
                ~category_id:cat_metabolomics.id ()
            in

            (* --- 3. Products --- *)
            let* prod_24 =
              Product.add ~name:"Corning 24-well Plate" ~brand:(Some "Corning")
                ~manufacturer_part_number:(Some "3524")
                ~description:(Some "Flat bottom 24-well")
            in

            let* _prod_48 =
              Product.add ~name:"Costar 48-well Plate" ~brand:(Some "Costar")
                ~manufacturer_part_number:(Some "3548")
                ~description:(Some "Standard 48-well")
            in

            let* prod_96 =
              Product.add ~name:"Greiner 96-well Plate" ~brand:(Some "Greiner")
                ~manufacturer_part_number:(Some "655090")
                ~description:(Some "Black 96-well")
            in

            let* prod_384 =
              Product.add ~name:"Nunc 384-well Plate" ~brand:(Some "Thermo")
                ~manufacturer_part_number:(Some "242765")
                ~description:(Some "Optical 384-well")
            in

            (* --- 4. Strains --- *)
            let* strain_mg1655 =
              Strain.add ~genus:"Escherichia" ~species:"coli"
                ~strain_name:"MG1655" ~genotype:(Some "WT") ~notes:None
                ~parent_strain_id:None
            in

            let* _link1 =
              Strain_external_link.add ~strain_id:strain_mg1655.id
                ~external_db_definition_id:db_ncbi.id ~value:"NC_000913.3"
            in

            let* strain_dh5a =
              Strain.add ~genus:"Escherichia" ~species:"coli"
                ~strain_name:"DH5-alpha" ~genotype:(Some "fhuA2 lacZ::T7 gene1")
                ~notes:None ~parent_strain_id:None
            in

            let* strain_pRS =
              Strain.add ~genus:"Escherichia" ~species:"coli"
                ~strain_name:"MG1655+pRS426"
                ~genotype:(Some "MG1655 pRS426 (Ura3+)") ~notes:None
                ~parent_strain_id:(Some strain_mg1655.id)
            in

            let* strain_yeast =
              Strain.add ~genus:"Saccharomyces" ~species:"cerevisiae"
                ~strain_name:"BY4741" ~genotype:(Some "MATa his3 leu2")
                ~notes:None ~parent_strain_id:None
            in

            (* --- 5. Projects & Data --- *)

            (* PROJECT 1: High-Throughput Screening *)
            let* p1 =
              Project.add ~name:"Induction Kinetics Alpha"
                ~description:
                  (Some "Screening small molecules for promoter induction.")
                ~owner:(Some "admin@exlab.com") ~status:Exlab_core.Types.Active
                ~contact_name:None ()
            in

            let* plate1_96 =
              Plate.add ~name:"Screening Run #01" ~project_id:p1.id
                ~product_id:(Some prod_96.id)
                ~plate_format:Exlab_core.Types.Well_96 ~category:"Experimental"
                ()
            in

            (* Create a master source sample for Project 1 *)
            let* p1_master_source =
              Sample.add ~project_id:p1.id ~sample_type:"Unknown"
                ~category:Exlab_core.Types.Source ~parent_sample_id:None
                ~strain_id:(Some strain_dh5a.id) ~community_id:None ()
            in

            (* Fill 48 wells in the 96-well plate using the source as parent *)
            let rec fill_96 row col count =
              if count > 48 then Lwt.return (Ok ())
              else
                let coord = Printf.sprintf "%c%d" (Char.chr (64 + row)) col in
                let* s =
                  Sample.add ~project_id:p1.id
                    ~sample_type:"Liquid Cell Culture"
                    ~category:Exlab_core.Types.Experimental
                    ~parent_sample_id:(Some p1_master_source.id) ~strain_id:None
                    ~community_id:None ()
                in
                let* () =
                  Well.assign_by_coordinate ~plate_id:plate1_96.id
                    ~coordinate:coord ~sample_id:(Some s.id)
                in
                let* _ =
                  Result_value.add ~parent:(Result_value.Sample s.id)
                    ~result_definition_id:rd_od600.id
                    ~value:
                      (Some
                         (Exlab_core.Types.ResultPayload.Float
                            (0.1 *. float_of_int count)))
                in

                let next_row = if col = 12 then row + 1 else row in
                let next_col = if col = 12 then 1 else col + 1 in
                fill_96 next_row next_col (count + 1)
            in
            let* () = fill_96 1 1 1 in

            (* PROJECT 2: Yeast Library HT *)
            let* p2 =
              Project.add ~name:"Yeast Knockout Library"
                ~description:(Some "Building a genome-wide knockout library.")
                ~owner:(Some "admin@exlab.com") ~status:Exlab_core.Types.Active
                ~contact_name:None ()
            in

            let* plate2_384 =
              Plate.add ~name:"Library Plate A1" ~project_id:p2.id
                ~product_id:(Some prod_384.id)
                ~plate_format:Exlab_core.Types.Well_384 ~category:"Source" ()
            in

            (* Fill 96 wells in the 384-well plate *)
            let rec fill_384 count =
              if count > 96 then Lwt.return (Ok ())
              else
                let coord = Printf.sprintf "A%d" count in
                let* s =
                  Sample.add ~project_id:p2.id ~sample_type:"Solid Cell Culture"
                    ~category:Exlab_core.Types.Source ~parent_sample_id:None
                    ~strain_id:(Some strain_yeast.id) ~community_id:None ()
                in
                let* () =
                  Well.assign_by_coordinate ~plate_id:plate2_384.id
                    ~coordinate:coord ~sample_id:(Some s.id)
                in
                fill_384 (count + 1)
            in
            let* () = fill_384 1 in

            (* PROJECT 3: Metabolic Extracts *)
            let* p3 =
              Project.add ~name:"Natural Product Discovery"
                ~description:
                  (Some "Isolating novel antibiotics from soil microbes.")
                ~owner:(Some "admin@exlab.com") ~status:Exlab_core.Types.Active
                ~contact_name:None ()
            in

            let* plate3_24 =
              Plate.add ~name:"Extraction Run X" ~project_id:p3.id
                ~product_id:(Some prod_24.id)
                ~plate_format:Exlab_core.Types.Well_24 ~category:"Experimental"
                ()
            in

            (* Create a source sample first *)
            let* s_src =
              Sample.add ~project_id:p3.id ~sample_type:"Unknown"
                ~category:Exlab_core.Types.Source ~parent_sample_id:None
                ~strain_id:(Some strain_pRS.id) ~community_id:None ()
            in

            let* s_ext =
              Sample.add ~project_id:p3.id ~sample_type:"Unknown"
                ~category:Exlab_core.Types.Experimental
                ~parent_sample_id:(Some s_src.id) ~strain_id:None
                ~community_id:None ()
            in

            let* () =
              Well.assign_by_coordinate ~plate_id:plate3_24.id ~coordinate:"A1"
                ~sample_id:(Some s_ext.id)
            in

            let* _ =
              Result_value.add ~parent:(Result_value.Sample s_ext.id)
                ~result_definition_id:rd_conc.id
                ~value:(Some (Exlab_core.Types.ResultPayload.Float 125.5))
            in

            let* _ =
              Result_value.add ~parent:(Result_value.Plate plate3_24.id)
                ~result_definition_id:rd_is_valid.id
                ~value:(Some (Exlab_core.Types.ResultPayload.Boolean true))
            in

            (* PROJECT 4: Internal Standards *)
            let* p4 =
              Project.add ~name:"Lab Quality Standards"
                ~description:
                  (Some "Standardized protocols and reference materials.")
                ~owner:(Some "admin@exlab.com") ~status:Exlab_core.Types.Active
                ~contact_name:None ()
            in

            (* Assign standard admin user (ID 1) to this project *)
            let* _ = Project_user.assign ~project_id:p4.id ~user_id:1 in

            Lwt.return (Ok ())
          in

          let* result = seed_operation in
          match result with
          | Ok () ->
              print_endline
                "✅ Comprehensive demo environment populated successfully!";
              Lwt.return_unit
          | Error err ->
              let msg =
                match err with
                | `Msg m -> m
                | `Bad_Request m -> m
                | `Not_Found m -> m
                | #Caqti_error.t as db_err -> Caqti_error.show db_err
                | _ -> "Unknown error"
              in
              prerr_endline ("❌ Error during comprehensive population: " ^ msg);
              Lwt.return_unit)
      | Ok projects ->
          Printf.printf
            "Database has %d projects already. Skipping comprehensive \
             population.\n"
            (List.length projects);
          Lwt.return_unit
      | Error err ->
          prerr_endline
            ("❌ Error checking database state: " ^ Caqti_error.show err);
          Lwt.return_unit)
  | _ -> Lwt.return_unit
