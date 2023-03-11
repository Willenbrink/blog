FROM ocaml/opam:opensuse-ocaml-5.1
COPY . app
WORKDIR "app/"
RUN opam pin ./hc --with-version=dev
RUN sudo zypper install -y libev-devel libopenssl-devel
RUN opam install -y dune dream js_of_ocaml brr js_of_ocaml-ppx
RUN opam install -y .
EXPOSE $PORT
CMD blog_server $PORT
