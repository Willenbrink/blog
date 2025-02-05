open Brr
open Brr_io
open Brr_webworkers
open Brr_canvas

type framebuffer =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t

let width, height = (300, 200)

let convert_from_tarray tarray : framebuffer =
  (Tarray.to_bigarray1 tarray
   |> Bigarray.genarray_of_array1
   |> Bigarray.reshape_2) width height

let convert_to_tarray bigarray =
  let h = Bigarray.Array2.dim1 bigarray in
  let w = Bigarray.Array2.dim2 bigarray in
  let data =
    Bigarray.reshape_1 (Bigarray.genarray_of_array2 bigarray) (w * h)
  in
  let data =
    let open Bigarray in
    data
    |> ( Tarray.of_bigarray1
         :    (int, int8_unsigned_elt, c_layout) Bigarray.Array1.t
           -> Tarray.uint8_clamped )
    |> Tarray.(of_tarray Uint8_clamped)
  in
  data

let create_array w h =
  let array : framebuffer =
    let init row col = match col mod 4 with 3 -> 255 | _ -> 0 in
    let open Bigarray in
    Array2.init Int8_unsigned C_layout h (w * 4) init
  in
  Console.log ["From within create_array: "; array];
  array

let write_to_array array row col (r, g, b) =
  Bigarray.Array2.set array row ((col * 4) + 0) r ;
  Bigarray.Array2.set array row ((col * 4) + 1) g ;
  Bigarray.Array2.set array row ((col * 4) + 2) b

let worker () =
  Fut.await (Ev.next Message.Ev.message G.target) (fun e ->
      let (array, w, h, num_rays, kernel_size) = Message.Ev.data (Ev.as_type e) in
      Console.log [array];
      Raytracer.main (write_to_array array) w h num_rays kernel_size ;
      Worker.G.post (array : framebuffer))

let update_canvas canvas (array_acc : framebuffer) (array_worker : framebuffer) =
  for row = 0 to height - 1 do
    for col = 0 to width * 4 - 1 do
      Console.log [array_acc; array_worker];
      let prev = Bigarray.Array2.get array_acc row col in
      let incr = Bigarray.Array2.get array_worker row col in
      Bigarray.Array2.set array_acc row col (prev + incr)
    done
  done;
  let data = convert_to_tarray array_acc in
  let image_data = C2d.Image_data.create ~data ~w:(width / 4) ~h:height () in
  let ctx = C2d.get_context canvas in
  C2d.put_image_data ctx image_data ~x:0 ~y:0

let raytrace_main canvas array =
  let start = Js_of_ocaml__Js.date##now in
  try
    let spawn_worker () =
      try Ok (Worker.create (Jstr.v "public/frontend.js"))
      with Jv.Error e -> Error e
    in
    Canvas.set_w canvas width ;
    Canvas.set_h canvas height ;
    let workers = List.init 1 (fun _ -> Result.get_ok (spawn_worker ())) in
    List.iter
      (fun worker ->
         let array = create_array width height in
         Worker.post worker (array, width, height, 1, 1))
      workers ;
    List.map Worker.as_target workers
    |> List.map (Ev.next Message.Ev.message)
    |> List.iter (fun msg ->
        Fut.await msg (fun ev ->
            (Message.Ev.data (Ev.as_type ev) : framebuffer)
            |> update_canvas canvas array ) );
    let end_ = Js_of_ocaml__Js.date##now in
    Console.(log [str "Raytracing finished in:"; end_ -. start; str "ms"])
  with e ->
    Console.(log [str "Exception encountered:"; str @@ Printexc.to_string e])

let main () =
  let canvas = Canvas.create [El.txt' "Javascript is needed to view this content."] in
  let array = create_array width height in
  let view = Canvas.to_el canvas in
  let () =
    match
      El.find_first_by_selector (Jstr.of_string "#clientside_raytracing")
    with
    | Some but ->
        Ev.listen Ev.click
          (fun _ -> raytrace_main canvas array)
          (El.as_target but)
        |> ignore
    | None ->
        Console.log ["Failed to find raytracing button"]
  in
  let children = [view] in
  El.fold_find_by_selector
    (fun el () -> El.set_children el children)
    (Jstr.of_string "#app") () ;
  Htmlact_page.init ()

let () = if Worker.ami () then worker () else main ()
