let () =
  let port =
    try Sys.argv.(1) |> int_of_string_opt |> Option.get with _ -> 8080
  in
  Dream.run ~port
  @@ Dream.logger
  (* @@ Dream_livereload.inject_script ()    (\* <-- *\) *)
  @@ Dream.router [
    Dream.get "/index.html" @@ Dream.static ".";
    Dream.get "/main.js" @@ Dream.static ".";
    (* Dream_livereload.route ();            (\* <-- *\) *)
  ]
