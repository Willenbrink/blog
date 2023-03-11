FROM ocaml/opam:opensuse-ocaml-5.0
# FROM ocaml/opam:debian-testing-ocaml-5.0
COPY . app
WORKDIR "app/"
RUN ls hc
# RUN opam pin add hc https://github.com/Willenbrink/hc.git.0.0.2
# RUN sudo zypper install -y libev-devel libopenssl-devel
# RUN sudo apt-get -y libev-dev libopenssl-dev
RUN sudo zypper install -y libev-devel libopenssl-devel
RUN opam install -y dune dream js_of_ocaml brr js_of_ocaml-ppx
RUN rm hc/dune-project
RUN opam install -y .
EXPOSE $PORT
CMD blog_server $PORT
