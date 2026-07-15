(lang dune 3.22)

(name exlab)

(generate_opam_files true)

(source
 (github exfab/exlab))

(authors "Andrew McNaughton <mcnaughtonadm@gmail.com>")

(maintainers "Andrew McNaughton <mcnaughtonadm@gmail.com>")

(license GPL-3.0-only)

(package
 (name exlab)
 (synopsis "A high-performance LIMS for synthetic biology")
 (description "Exlab is a high-performance Laboratory Information Management System (LIMS) designed for synthetic biology and high-throughput laboratories, built with OCaml.")
 (depends
  ocaml
  dune
  dream
  lwt
  caqti
  caqti-lwt
  caqti-driver-postgresql
  ppx_yojson_conv
  uuidm
  base
  yojson
  ppx_deriving
  csv-lwt
  passe
  (alcotest :with-test)
  (alcotest-lwt :with-test)
  (bisect_ppx :with-test)
  )
 (tags
  (lims "synthetic biology" ocaml dream caqti)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
