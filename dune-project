(lang dune 3.16)

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
 (depends ocaml dune (ocamlformat :with-test) )
 (tags ()))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
