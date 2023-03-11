open Brr

type framebuffer = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t

let convert_to_img_data bigarray =
  let h = Bigarray.Array2.dim1 bigarray in
  let w = Bigarray.Array2.dim2 bigarray in
  let data =
    Bigarray.reshape_1 (Bigarray.genarray_of_array2 bigarray) (w * h)
  in
  let data =
    let open Bigarray in
    data
    |> (Tarray.of_bigarray1 : (int, int8_unsigned_elt, c_layout) Bigarray.Array1.t -> Tarray.uint8_clamped)
    |> Tarray.(of_tarray Uint8_clamped)
  in
  Brr_canvas.C2d.Image_data.create ~data ~w:(w / 4) ~h ()

let worker () =
  let open Brr_io in
  let open Brr_webworkers in
  let apply (w,h,num_rays,kernel_size) =
    let array : framebuffer =
      let init row col = match col mod 4 with
        | 3 -> 255
        | _ -> 0
      in
      let open Bigarray in
      Array2.init Int8_unsigned C_layout h (w * 4) init
    in
    let write_to_array row col (r,g,b) =
      Bigarray.Array2.set array row (col * 4 + 0) r;
      Bigarray.Array2.set array row (col * 4 + 1) g;
      Bigarray.Array2.set array row (col * 4 + 2) b;
      ()
    in
    Raytracer.main write_to_array w h num_rays kernel_size;
    Worker.G.post (array : framebuffer)
  in
  Fut.await (Ev.next Message.Ev.message G.target) (fun e -> apply (Message.Ev.data (Ev.as_type e)))

let update_canvas canvas (array : framebuffer) =
  (* Console.(log [str "Raytracing done"; array]); *)
  let data = convert_to_img_data array in
  let ctx = Brr_canvas.C2d.get_context canvas in
  Brr_canvas.C2d.put_image_data ctx data ~x:0 ~y:0;
  ()

let raytrace_main canvas w h () =
  let start = Js_of_ocaml__Js.date##now in
  try
    let spawn_worker () =
      try Ok (Brr_webworkers.Worker.create (Jstr.v "public/frontend.js")) with
      | Jv.Error e -> Error e
    in
    let workers = List.init 1 (fun _ -> match spawn_worker () with
        | Error e -> failwith "Worker init failed"
        | Ok w -> w
      )
    in
    List.iter (fun worker ->
        let open Brr_io in
        let open Brr_webworkers in
        (* let msg = Ev.next Message.Ev.message (Worker.as_target w) in *)
        (* Console.log [Console.str "Raytring begin"; array]; *)
        Worker.post worker (w, h, 2, 1);
        let msg = Ev.next Message.Ev.message (Worker.as_target worker) in
        Fut.await msg (fun ev ->
            Brr_canvas.Canvas.set_w canvas w;
            Brr_canvas.Canvas.set_h canvas h;
            (Message.Ev.data (Ev.as_type ev) : framebuffer)
            |> update_canvas canvas;
            let end_ = Js_of_ocaml__Js.date##now in
            Console.(log [str "Raytracing finished in:"; end_ -. start; str "ms"]);
          );
        ()
      ) workers
  with
  | e ->
    Console.(log [str "Exception encountered:"; str @@ Printexc.to_string e])

let main () =
  let w,h = 600, 400 in
  let canvas = Brr_canvas.Canvas.create [El.txt' "Javascript is needed to view this content."] in
  let view = Brr_canvas.Canvas.to_el canvas in
  let () =
    (* let but = El.button [El.txt (Jstr.v label)] in *)
    match El.find_first_by_selector (Jstr.of_string "#clientside_raytracing") with
    | Some but ->
      Ev.listen Ev.click (fun _ -> raytrace_main canvas w h ()) (El.as_target but)
      |> ignore
    | None ->
      Console.log ["Failed to find raytracing button"];
  in
  let children = [view] in
  El.fold_find_by_selector (fun el () -> El.set_children el children) (Jstr.of_string "#app") ();
  Hc_page.init ()

let () = if Brr_webworkers.Worker.ami () then worker () else main ()
