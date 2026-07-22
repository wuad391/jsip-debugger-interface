(lang dune 3.0)

(generate_opam_files true)

(maintainers "TODO")

(authors "Clara Yee and Robyn Wu")

(license MIT)

(package
 (name jsip-debugger-interface)
 (synopsis "An interface for examining the behavior of OCaml programs")
 (depends
  (ocaml
   (>= 5.2.0))
  dune
  ocaml-lsp-server
  ocamlformat
  (odoc :with-doc)
  core
  ppx_jane
  (expect_test_helpers_core :with-test)))
