(lang dune 3.7)

(name blog)

(generate_opam_files true)

(source
 (github Willenbrink/blog))

(authors "Sebastian Willenbrink")

(maintainers "Sebastian Willenbrink")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name blog)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune dream hc_page)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
