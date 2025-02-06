let raytracer_html request =
  let open Dream_html in
  let open HTML in
  let slider name min_ max_ value_ =
    let my_id = "raytrace_param_" ^ name in
    div [] [
      input [type_ "range"; id "raytrace_param_%s" name; min "%i" min_; max "%i" max_; value "%i" value_];
      label [] [txt "%s" name]
    ]
  in
  respond @@
  html [lang "en"] [
    head [charset "utf-8"; name "viewport"; content "width=device-width,initial-scale=1.0"] [
      title [] "Raytracing in a weekend";
      script [
        src "https://unpkg.com/htmx.org@2.0.4/dist/htmx.js";
        integrity "sha384-oeUn82QNXPuVkGCkcrInrS1twIxKhkZiFfr2TdiuObZ3n3yIeMiqcRzkIcguaof1";
        crossorigin `anonymous
      ] ""
    ];
    body [] [
      h1 [] [txt "Raytracing"];
      div [] [
        button [Hx.get "raytracing"; Hx.target "next #image"] [txt "Raytrace"];
           slider "width" 1 1000 300;
           slider "height" 1 1000 200;
           slider "rays per pixel" 1 100 3;
           slider "kernelsize" 0 5 0;
        div [id "image"] [noscript [] [txt "You need to enable Javascript to see this content."];
        ]
      ]
    ]
  ]

let raytracer_render oc =
  let w,h = 600,400 in
  let img = Image.create_rgb w h in
  let write_to_array row col (r,g,b) =
    Image.write_rgb img col row r g b
  in
  print_endline "Raytracing!";
  Raytracer.main write_to_array w h 1 1;
  let buffer = Buffer.create (w * h) in
  img
  |> ImageLib.writefile ~extension:"png" @@ ImageUtil.chunk_writer_of_buffer buffer;
  print_endline "Raytracing done!";
  Buffer.output_buffer oc buffer


let components = [
  "raytracing"
]

let%path index = "/index.html"

let () =
  let port =
    try Sys.argv.(1) |> int_of_string_opt |> Option.get with _ -> 8080
  in
  let raytracing_out = ref "" in
  Dream.run ~interface:"0.0.0.0" ~port
  @@ Dream.logger
  (* @@ Dream_livereload.inject_script ()    (\* <-- *\) *)
  @@ Dream.router [
    Dream_html.get index raytracer_html;
    Dream.get "/public/**" @@ Dream.static "../_build/default/blog/public/";
    Dream.get "raytracing.png" @@ (fun ev ->
      Dream.from_filesystem "" !raytracing_out ev);
    Dream.get "raytracing" @@ (fun req ->
        let queries = Dream.all_queries req in
        List.iter (fun (a,b) -> Printf.printf "%s\n%s\n\n" a b) @@ ("abc","def")::queries;
        let file,oc = Filename.open_temp_file ~mode:[Open_binary] "raytracing_output" ".png" in
        raytracing_out := file;
        raytracer_render oc;
        close_out oc;
        Dream_html.HTML.(img [src "raytracing.png"; alt "Raytraced image"])
        |> Dream_html.respond
      );
    (* Dream_livereload.route ();            (\* <-- *\) *)
  ]
