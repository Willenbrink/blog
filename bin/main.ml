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

let measure_time name f =
  let start = Js_of_ocaml__Js.date##now in
  f ();
  let end_ = Js_of_ocaml__Js.date##now in
  Console.(log [str name; str "finished in:"; end_ -. start; str "ms"]);
  ()

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
    Raytracer.main array w h num_rays kernel_size;
    Worker.G.post (array : framebuffer)
  in
  Fut.await (Ev.next Message.Ev.message G.target) (fun e -> apply (Message.Ev.data (Ev.as_type e)))

let update_canvas canvas (array : framebuffer) =
  (* Console.(log [str "Raytracing done"; array]); *)
  let data = convert_to_img_data array in
  let ctx = Brr_canvas.C2d.get_context canvas in
  Brr_canvas.C2d.put_image_data ctx data ~x:0 ~y:0;
  ()

let raytrace_main canvas () =
  measure_time "Raytracing" (fun () ->
    try
      let spawn_worker () =
        try Ok (Brr_webworkers.Worker.create (Jstr.v "main.js")) with
        | Jv.Error e -> Error e
      in
      let workers = List.init 1 (fun _ -> match spawn_worker () with
          | Error e -> failwith "Worker init failed"
          | Ok w -> w
        )
      in
      List.iter (fun w ->
        let open Brr_io in
        let open Brr_webworkers in
        (* let msg = Ev.next Message.Ev.message (Worker.as_target w) in *)
        (* Console.log [Console.str "Raytring begin"; array]; *)
        Worker.post w (300, 200, 1, 0);
        let msg = Ev.next Message.Ev.message (Worker.as_target w) in
        Fut.await msg (fun ev ->
            (Message.Ev.data (Ev.as_type ev) : framebuffer)
            |> update_canvas canvas
          );
        ()
      ) workers
    with
    | e ->
      Console.(log [str "Exception encountered:"; str @@ Printexc.to_string e])
  )


let main () =
  let w,h = 900, 600 in
  let h1 = El.h1 [El.txt' "Media test"] in
  let info = El.txt' "Media information is dumped in the browser console."in
  let stream = ref None in
  let canvas = Brr_canvas.Canvas.create ~w ~h [El.txt' "Javascript is needed to view this content."] in
  let view = Brr_canvas.Canvas.to_el canvas in
  let cam = Util.button (Media.test_stream ~view `Camera stream) "Open camera" in
  let screen = Util.button (Media.test_stream ~view `Screen stream) "Share screen" in
  let raytrace_b = Util.button (raytrace_main canvas) "Run Raytracing" in
  let children = [h1; El.p [info]; El.p [cam; screen; raytrace_b]; view] in
  raytrace_main canvas ();
  El.set_children (Document.body G.document) children

let () = if Brr_webworkers.Worker.ami () then worker () else main ()
