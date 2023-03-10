let string_of_html html =
  Format.asprintf "%a" (Tyxml.Html.pp ()) html

let error_template error debug_dump suggested_response =
  let status = Dream.status suggested_response in
  let error_code = Dream.status_to_int status
  and reason = Dream.status_to_string status in

  Dream.set_header suggested_response "Content-Type" Dream.text_html;
  Dream.set_body suggested_response
    begin
      let open Tyxml_html in
      html
        (head (title @@ txt @@ Printf.sprintf "%i - %s" error_code reason) [])
        (body [
              pre [ txt debug_dump]
          ])
    |> string_of_html
    end;
  suggested_response |> Lwt.return

let raytracer_html =
  let open Tyxml_html in
  html ~a:[a_lang "en"]
    (head
       (title (txt "Raytracing in a weekend"))
       [
         (meta ~a:[a_charset "utf-8"; ] ());
         (meta ~a:[a_name "viewport"; a_content "width=device-width,initial-scale=1.0" ] ());
         (script ~a:[a_defer (); a_src "public/frontend.js"] (txt ""));
       ]
    )
    (body [
      (h1 [txt "Raytracing"]);
      (div ~a:[a_id "app"]
         [noscript [txt "You need to enable Javascript to see this content."]]
      )

    ])

let components = [
  "raytracing"
]

let () =
  let port =
    try Sys.argv.(1) |> int_of_string_opt |> Option.get with _ -> 8080
  in
  Dream.run ~interface:"0.0.0.0" ~port ~error_handler:(Dream.error_template error_template)
  @@ Dream.logger
  (* @@ Dream_livereload.inject_script ()    (\* <-- *\) *)
  @@ Dream.router [
    Dream.get "/index.html" @@ (fun _ -> Dream.html @@ string_of_html raytracer_html);
    Dream.get "/public/**" @@ Dream.static "_build/default/public/";
    (* Dream_livereload.route ();            (\* <-- *\) *)
  ]
