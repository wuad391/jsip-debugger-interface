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
  async
  async_js
  bonsai
  bonsai_term
  bonsai_web
  cohttp-async
  core
  core_unix
  js_of_ocaml
  js_of_ocaml-ppx
  ocaml-embed-file
  ppx_html
  ppx_jane
  uri
  virtual_dom
  (expect_test_helpers_core :with-test)
  (notty-community :with-test)))
