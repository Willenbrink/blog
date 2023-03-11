FROM ocaml/opam:opensuse-ocaml-4.14
# FROM ocaml/opam:debian-testing-ocaml-5.0
COPY . app
WORKDIR "app/"
RUN ls hc
RUN sudo rm hc/dune-project
# RUN opam pin ./hc -w
RUN sudo zypper install -y libev-devel libopenssl-devel
# RUN sudo apt-get -y libev-dev libopenssl-dev
RUN opam install -y dune dream js_of_ocaml brr js_of_ocaml-ppx
RUN opam install -y .
EXPOSE $PORT
CMD blog_server $PORT
