FROM ocaml/opam:opensuse-ocaml-4.14
# FROM ocaml/opam:debian-testing-ocaml-5.0
COPY . app
WORKDIR "app/"
# RUN sudo rm hc/dune-project
RUN sudo mkdir _build; sudo chown opam:opam --recursive _build
# RUN opam pin ./hc -w
RUN sudo zypper install -y libev-devel libopenssl-devel
# RUN sudo apt-get -y libev-dev libopenssl-dev
RUN opam pin git+ssh://git@github.com/ahrefs/tyxml.git#ocaml5
RUN opam install -y dune dream js_of_ocaml brr js_of_ocaml-ppx tyxml-ppx
# RUN opam install -y .
RUN eval $(opam env); dune build
EXPOSE $PORT
CMD app/_build/default/server/server.exe $PORT
