(lang dune 3.17)

(name stc)

(generate_opam_files true)

(source
 (github kai-pischke/session-types))

(authors "Kai Pischke <kai.pischke@cs.ox.ac.uk>")

(maintainers "Maintainer Name <kai.pischke@cs.ox.ac.uk>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name stc)
 (synopsis "A tool which implements multiparty session types.")
 (description "A longer description")
 (depends ocaml)
 (tags
  ("add topics" "to describe" your project)))

(using menhir 2.1)

