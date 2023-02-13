(lang dune 3.6)

(name GoCaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name GoCaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  (base
   (>= 4.13))
  (ppx_inline_test
   (>= 4.13))
  (ppx_jane
   (>= 4.13))
  (ppx_assert
   (>= 0.15.0)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
