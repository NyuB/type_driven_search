(lang dune 3.22)

(name type_driven_search)

(generate_opam_files true)

(source
 (github NyuB/type_driven_search))

(authors "NyuB")

(maintainers "NyuB")

(license LICENSE)

(documentation https://github.com/NyuB/type_driven_search)

(package
 (name type_driven_search)
 (synopsis "Type driven search tools for C/C++")
 (description "Type driven search tools for C/C++")
 (depends
  ; build dependencies
  (ocaml
   (>= 5.4))
  (dune
   (>= 3.22))
  ; regular dependencies
  (ppx_deriving_yaml
   (>= 0.4))
  ; test dependencies
  (alcotest
   (and
    :with-test
    (>= 1.9)))
  (ocamlformat
   (and
    :with-test
    (>= 0.29)))
  (ppx_expect
   (and
    :with-test
    (>= 0.17))))
 (tags ()))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
