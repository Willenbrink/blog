FROM ocaml/opam:opensuse-ocaml-4.14
COPY . app
WORKDIR "app/"
RUN sudo zypper install -y libev-devel libopenssl-devel
# RUN opam pin hc_page
RUN opam install -y dune dream js_of_ocaml brr js_of_ocaml-ppx
# RUN git submodule update --init --recursive
RUN opam install -y .
EXPOSE $PORT
CMD blog_server $PORT
