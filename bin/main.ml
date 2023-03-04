open Brr

let convert_to_img_data bigarray =
  let w = Bigarray.Array2.dim1 bigarray in
  let h = Bigarray.Array2.dim2 bigarray in
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

let raytrace_main canvas () =
  let init y x = match x mod 4 with
    | 3 -> 255
    | _ -> 0
  in
  let open Bigarray in
  let open Brr_canvas.Canvas in
  let array = Array2.init Int8_unsigned C_layout (4 * w canvas) (h canvas) init in
  Raytracer.main array;
  let data = convert_to_img_data array in
  let ctx = Brr_canvas.C2d.get_context canvas in
  Brr_canvas.C2d.put_image_data ctx data ~x:0 ~y:0


let main () =
  let w,h = 600, 400 in
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

let () = main ()
