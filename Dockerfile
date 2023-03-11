FROM ocaml/opam:opensuse-ocaml-5.0

COPY . app
WORKDIR "app/"

RUN sudo zypper update
RUN sudo zypper install -y opam

RUN sudo mkdir _build; sudo chown opam:opam --recursive _build
RUN opam pin ./hc --with-version=dev
RUN opam pin https://github.com/talex5/dream.git --with-version=eio
RUN opam pin https://github.com/ahrefs/tyxml.git --with-version=ocaml5

RUN sudo zypper install -y libev-devel libopenssl-devel

RUN opam install -y dune js_of_ocaml brr js_of_ocaml-ppx vector3 imagelib
RUN opam install -y .

# RUN eval $(opam env); dune build
EXPOSE $PORT
CMD blog_server $PORT
