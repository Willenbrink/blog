FROM ocaml/opam:opensuse-ocaml-4.14

COPY . app
WORKDIR "app/"

# RUN sudo rm hc/dune-project
RUN sudo mkdir _build; sudo chown opam:opam --recursive _build
RUN opam pin ./hc --with-version=dev
RUN opam pin ./tyxml --with-version=dev

RUN sudo zypper install -y libev-devel libopenssl-devel

RUN opam install -y dune dream js_of_ocaml brr js_of_ocaml-ppx vector3 imagelib
# RUN opam install -y .

RUN eval $(opam env); dune build
EXPOSE $PORT
CMD app/_build/default/server/server.exe $PORT
