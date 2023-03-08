let () =
  Dream.run
  @@ Dream.logger
  (* @@ Dream_livereload.inject_script ()    (\* <-- *\) *)
  @@ Dream.router [
    Dream.get "/index.html" @@ Dream.static ".";
    Dream.get "/main.js" @@ Dream.static ".";
    (* Dream_livereload.route ();            (\* <-- *\) *)
  ]
