(lang dune 3.7)

(name blog)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name blog_server)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune dream)
 (tags
  (topics "to describe" your project)))

(package
 (name blog_frontend)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune js_of_ocaml brr raytracer util_js js_of_ocaml-ppx
 )
 (tags
  (topics "to describe" your project)))


; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
