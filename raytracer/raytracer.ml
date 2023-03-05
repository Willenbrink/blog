let rng = ref (fun () -> 0.)

let print_chance chance printf =
  if !rng () < chance
  then printf ()

module Vec = struct
  include Vector3
  open Vector3

  module Syntax = struct
    let (+|) = add
    let (-|) a b = { x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z }
    let ( *|) = mult
    let (/|) = div
  end
end
type vec = Vec.t = {x : float; y : float; z : float}

open Vec.Syntax

module Ray = struct
  type t = { pos: Vec.t; dir: Vec.t }

  let at t {pos; dir} = pos +| dir *| t
end
type ray = Ray.t = {pos: Vec.t; dir: Vec.t}

type hit = { pos : Vec.t; normal : Vec.t; dist : float; front_face : bool }

module Sphere = struct
  type t = { center: Vec.t; radius: float; color: Vec.t }

  let hit_dist {center; radius; _} {pos; dir} t_min t_max =
    let pc = pos -| center in
    let a = Vec.mag2 dir in
    let half_b = Vec.dot pc dir in
    let c = Vec.mag2 pc -. radius *. radius in
    let discr = half_b*.half_b -. a*.c in
    if discr < 0.
    then None
    else
      let sqrtd = sqrt discr in
      let root1 = ( (-.half_b) -. sqrtd) /. a in
      let root2 = ( (-.half_b) +. sqrtd) /. a in
      match t_min < root1 && root1 < t_max, t_min < root2 && root2 < t_max with
      | true,_ -> Some root1
      | _, true -> Some root2
      | _ -> None

  let rec random_in_sphere ({center; radius; _} as s) =
    let r1 = !rng () *. radius *. 2. in
    let r2 = !rng () *. radius *. 2. in
    let r3 = !rng () *. radius *. 2. in
    let rand = Vec.make r1 r2 r3 in
    if Vec.mag2 rand >= 1.
    then random_in_sphere s
    else center +| rand

  let rec random_in_unit_sphere () =
    let r1 = !rng () *. 2. -. 1. in
    let r2 = !rng () *. 2. -. 1. in
    let r3 = !rng () *. 2. -. 1. in
    let rand = Vec.make r1 r2 r3 in
    if Vec.mag2 rand >= 1.
    then random_in_unit_sphere ()
    else rand

  let random_on_unit_sphere () =
    Vec.normalize (random_in_unit_sphere ())
end

module Camera = struct
  type t = {pos: Vec.t; ll_corner: Vec.t; horizontal: Vec.t; vertical: Vec.t }

  let make w h =
    let h_vp = 2. in
    let w_vp = w /. h *. h_vp in
    let focal_length = 1. in

    let pos = Vec.make 0. 0. 0. in
    let horizontal = Vec.make w_vp 0. 0. in
    let vertical = Vec.make 0. h_vp 0. in
    let ll_corner = pos -| horizontal /| 2. -| vertical /| 2. -| (Vec.make 0. 0. focal_length) in
    { pos; ll_corner; horizontal; vertical; }

  let ray {pos; ll_corner; horizontal; vertical; } u v =
    {
      pos;
      dir = Vec.normalize @@ ll_corner  +| horizontal *| u +| vertical *| v -| pos
    }
end

type sphere = Sphere.t = { center: Vec.t; radius: float; color: Vec.t }

let normal ({center; radius; _}) r dist =
  let normal = Vec.normalize ((Ray.at dist r) -| center) in
  let front_face = Vec.dot r.dir normal < 0. in
  if front_face
  then normal, true
  else normal *| (-1.), false


let ray_color world r =
  let rec hit r =
    let f acc el =
      let max_dist = match acc with
        | None -> Float.infinity
        | Some { dist; _ } -> dist
      in
      match Sphere.hit_dist el r 0.0001 max_dist with
      | None -> acc
      | Some dist ->
        let normal, front_face = normal el r dist in
        Some { pos=Ray.at dist r; normal; dist; front_face }
    in
    List.fold_left f None world
  and ray_color r count hit_data = match (count, hit_data) with
    | 0,_ | _,None ->
      let unit = Vec.normalize r.dir in
      let t = 0.5 *. (unit.y +. 1.) in
      Vec.(make 1. 1. 1.) *| (1. -. t) +| Vec.(make 0.5 0.7 1.) *| t
    | count, Some { pos; normal; dist; front_face } ->
      (* let vec_refl = {pos; dir = r.dir +| normal *| 2.} in *)
      (* let color_refl = ray_color vec_refl (count - 1) (hit vec_refl) in *)

      let vec_diff = {pos; dir = normal +| Sphere.random_on_unit_sphere ()} in
      let color_diff = ray_color vec_diff (count - 1) (hit vec_diff) in
      (* color_refl *| 0.5 *)
      color_diff *| 0.5
      (* (norm +| Vec.(make 1. 1. 1.)) *| 0.5 *)
  in
  ray_color r 20 (hit r)


let write_color array row col (color : vec) num_samples =
  let scale = 1. /. num_samples in
  let color_of_float float =
    int_of_float @@ sqrt (scale *. float) *. 255.
  in
  let r = color_of_float color.x in
  let g = color_of_float color.y in
  let b = color_of_float color.z in
  begin
    try
      Bigarray.Array2.set array row (4*col+0) r;
      Bigarray.Array2.set array row (4*col+1) g;
      Bigarray.Array2.set array row (4*col+2) b;
      ()
    with e -> Printf.printf "col %i(%i), row %i" col (4*col) row; raise e
  end


let main rng_arg array (w_i,h_i) sqrt_samples_per_pixel =
  rng := rng_arg;
  let w, h = float_of_int w_i, float_of_int h_i in
  let cam = Camera.make w h in
  let world = [
    {center = (Vec.make 0. 0. (-1.)); radius = 0.5; color = (Vec.make 1. 0. 0.)};
    (* {center = (Vec.make 0. c (-1.)); radius = (c -. 0.5); color = (Vec.make 1. 0. 0.)}; *)
    (* {center = (Vec.make 0. (-10.5) (-1.)); radius = 9.; color = (Vec.make 0. 1. 0.)}; *)
    {center = (Vec.make 0. (100.5) (-1.)); radius = 100.; color = (Vec.make 0. 1. 0.)};

  ] in
  for row = 0 to h_i - 1 do
    for col = 0 to w_i - 1 do
      let color = ref (Vec.make 0. 0. 0.) in
      for sample1 = 0 to sqrt_samples_per_pixel - 1 do
        for sample2 = 0 to sqrt_samples_per_pixel - 1 do
          let u = ((float_of_int sample1) *. 0.1 +. (float_of_int col)) /. (w -. 1.) in
          let v = ((float_of_int sample2) *. 0.1 +. (float_of_int row)) /. (h -. 1.) in

          let r = Camera.ray cam u v in

          color := !color +| (ray_color world r);
        done;
      done;
      (* print_chance 0.0001 (fun () -> Printf.printf "%f %f %f\n" r.dir.x r.dir.y r.dir.z); *)
      let samples_per_pixel = float_of_int (sqrt_samples_per_pixel * sqrt_samples_per_pixel) in
      write_color array row col !color samples_per_pixel
    done;
    Brr.Console.log [row];
  done;

  ()
