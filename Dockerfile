FROM ocaml/opam:opensuse-ocaml-5.1
COPY . app
WORKDIR "app/"
RUN opam install -y .
CMD dune exec server/server.exe
