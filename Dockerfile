FROM ocaml/opam:opensuse-ocaml-4.14
COPY . app
WORKDIR "app/"
RUN sudo zypper install -y libev-devel libopenssl-devel
RUN opam install -y ocaml dune dream js_of_ocaml brr js_of_ocaml-ppx
RUN eval $(opam env)
RUN dune build
CMD blog_server
