# FROM ocaml/opam:opensuse-ocaml-5.0
FROM ocamlpro/ocaml:latest

COPY . app
WORKDIR "app/"

RUN sudo chown ocaml:ocaml --recursive . || true
RUN opam switch create default ocaml-system
#--deps --locked

RUN opam --version
# RUN sudo zypper update -y
# RUN sudo zypper install -y opam

RUN sudo mkdir _build && sudo chown ocaml:ocaml --recursive _build || true
RUN opam pin ./hc -w
RUN opam pin https://github.com/talex5/dream.git -w
RUN opam pin https://github.com/ahrefs/tyxml.git -w

# RUN sudo zypper install -y libev-devel libopenssl-devel

RUN opam install -y dune js_of_ocaml brr js_of_ocaml-ppx vector3 imagelib
RUN opam install -y .

# RUN eval $(opam env); dune build
EXPOSE $PORT
CMD blog_server $PORT
