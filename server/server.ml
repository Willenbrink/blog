let string_of_html html =
  Format.asprintf "%a" (Tyxml.Html.pp_elt ()) html

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
  let open Tyxml in
  let slider name min max value =
    let id = "raytrace_param_" ^ name in
    [%html {|
<div>
<input type=range id="id" name="id" min="min" max="max" value="value">
<label>|}[txt name]{|</label>
</div>
|}]
  in
  let button_target = ":up #image" in
  let button_query = ":up #raytrace_param_width" in
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
      (div
         [
           [%html "<button _data-request=raytracing _data-target="button_target" _data-query="button_query"> Serverside raytracing </button>"];
           [%html "<button id=clientside_raytracing> Clientside raytracing </button>"];
           slider "width" 1 1000 300;
           slider "height" 1 1000 200;
           slider "rays per pixel" 1 100 3;
           slider "kernelsize" 0 5 0;
           (div [
               (div ~a:[a_id "image"] [noscript [txt "You need to enable Javascript to see this content."]]);
               (div ~a:[a_id "app"] [noscript [txt "You need to enable Javascript to see this content."]]);
             ])
         ])
    ])

let raytracer_render oc =
  let w,h = 600,400 in
  let img = Image.create_rgb w h in
  let write_to_array row col (r,g,b) =
    Image.write_rgb img col row r g b
  in
  print_endline "Raytracing!";
  Raytracer.main write_to_array w h 10 1;
  let buffer = Buffer.create (w * h) in
  img
  |> ImageLib.writefile ~extension:"png" @@ ImageUtil.chunk_writer_of_buffer buffer;
  print_endline "Raytracing done!";
  Buffer.output_buffer oc buffer


let components = [
  "raytracing"
]

let () =
  let port =
    try Sys.argv.(1) |> int_of_string_opt |> Option.get with _ -> 8080
  in
  let raytracing_out = ref "" in
  Dream.run ~interface:"0.0.0.0" ~port ~error_handler:(Dream.error_template error_template)
  @@ Dream.logger
  (* @@ Dream_livereload.inject_script ()    (\* <-- *\) *)
  @@ Dream.router [
    Dream.get "/index.html" @@ (fun _ -> Dream.html @@ string_of_html raytracer_html);
    Dream.get "/public/**" @@ Dream.static "_build/default/public/";
    Dream.get "raytracing.png" @@ (fun ev ->
      Dream.from_filesystem "" !raytracing_out ev);
    Dream.get "raytracing" @@ (fun req ->
        let queries = Dream.all_queries req in
        List.iter (fun (a,b) -> Printf.printf "%s\n%s\n\n" a b) @@ ("abc","def")::queries;
        let file,oc = Filename.open_temp_file ~mode:[Open_binary] "raytracing_output" ".png" in
        raytracing_out := file;
        raytracer_render oc;
        close_out oc;
        Tyxml.Html.(img ~src:"raytracing.png" ~alt:"Raytraced image" ())
        |> string_of_html
        |> Dream.html
      );
    (* Dream_livereload.route ();            (\* <-- *\) *)
  ]
