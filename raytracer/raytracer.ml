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

  let near_zero t =
    abs_float t.x <= Float.epsilon *. 4.
    || abs_float t.y <= Float.epsilon *. 4.
    || abs_float t.z <= Float.epsilon *. 4.

  let albedo_correct t =
    { x = sqrt t.x; y = sqrt t.y; z = sqrt t.z; }

  let random a b c =
    { x = a *. !rng (); y = b *. !rng (); z = c *. !rng (); }
end
type vec = Vec.t = {x : float; y : float; z : float}

open Vec.Syntax

module Ray = struct
  type t = { pos: Vec.t; dir: Vec.t }

  let at t {pos; dir} = pos +| dir *| t
end
type ray = Ray.t = {pos: Vec.t; dir: Vec.t}

type material =
  | Diffuse of Vec.t
  | Metal of Vec.t
  | FuzzyMetal of Vec.t * float
  | Dielectric of float

type hit = { pos : Vec.t; normal : Vec.t; dist : float; front_face : bool; mat : material }

let rec random_in_unit_disc () =
  let r1 = !rng () *. 2. -. 1. in
  let r2 = !rng () *. 2. -. 1. in
  if (r1 *. r1) +. (r2 *. r2) >= 1.
  then random_in_unit_disc ()
  else r1, r2

module Sphere = struct
  type t = { center: Vec.t; radius: float; mat: material }

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
  type t = {
    pos: Vec.t; ll_corner: Vec.t;
    horizontal: Vec.t; vertical: Vec.t;
    u: Vec.t; v: Vec.t; w: Vec.t;
    lens_radius: float;
  }

  let make lookfrom lookat vup vfov aspect_ratio aperture focus_dist =
    let theta = vfov /. 360. *. (2. *. Float.pi) in
    let h = Float.tan (theta /. 2.) in
    let h_vp = 2. *. h in
    let w_vp = aspect_ratio *. h_vp in

    let w = Vec.normalize (lookfrom -| lookat) in
    let u = Vec.(normalize (cross vup w)) in
    let v = Vec.cross u w in

    let pos = lookfrom in
    let horizontal = u *| w_vp *| focus_dist in
    let vertical = v *| h_vp *| focus_dist in
    let ll_corner = pos -| horizontal /| 2. -| vertical /| 2. -| w *| focus_dist in
    let lens_radius = aperture /. 2. in
    { pos; ll_corner; horizontal; vertical; u; v; w; lens_radius; }

  let ray {pos; ll_corner; horizontal; vertical; u; v; w; lens_radius; } s t =
    let rd_x, rd_y = random_in_unit_disc () in
    let rd_x, rd_y = rd_x *. lens_radius, rd_y *. lens_radius in
    (* let rd_x, rd_y = 0., 0. in *)
    let pos = pos +| u *| rd_x +| v *| rd_y in
    {
      pos;
      dir = Vec.normalize @@ ll_corner  +| horizontal *| s +| vertical *| t -| pos;
    }
end

type sphere = Sphere.t = { center: Vec.t; radius: float; mat: material }

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
        Some { pos=Ray.at dist r; normal; dist; front_face; mat = el.mat }
    in
    List.fold_left f None world
  and ray_color r count hit_data = match (count, hit_data) with
    | 0,_ | _,None ->
      let unit = Vec.normalize r.dir in
      let t = 0.5 *. (unit.y +. 1.) in
      Vec.(make 1. 1. 1.) *| (1. -. t) +| Vec.(make 0.5 0.7 1.) *| t
    | count, Some { pos; normal; dist; front_face; mat} ->
      let mat_color = match mat with
        | Diffuse c
        | Metal c
        | FuzzyMetal (c,_) -> c
        | Dielectric _ -> Vec.make 1. 1. 1.
      in
      let reflect fuzz =
        {pos; dir = r.dir +| normal *| 2. +| Sphere.random_on_unit_sphere () *| fuzz}
      in
      let ray = match mat with
        | Diffuse _ ->
          (* TODO in theory, dir might be zero. This should be avoided if it really is a problem *)
          {pos; dir = normal +| Sphere.random_on_unit_sphere ()}
        | Metal _ ->
          reflect 0.
        | FuzzyMetal (_, f) ->
          reflect f
        | Dielectric index_of_refr ->
          let unit_dir = Vec.normalize r.dir in
          let refr_ratio = if front_face then (1. /. index_of_refr) else index_of_refr in

          let cos_theta = Float.min (Vec.dot (unit_dir *| (-1.)) normal) 1. in
          let sin_theta = sqrt (1. -. cos_theta *. cos_theta) in

          let reflectance =
            let r0 = Float.pow ((1. -. refr_ratio) /. (1. +. refr_ratio)) 2. in
            r0 +. (1. -. r0) *. Float.pow (1. -. cos_theta) 5.
          in

          if refr_ratio *. sin_theta > 1. || reflectance > !rng ()
          then
            reflect 0.
          else
            let refract =
              let r_out_perp = (unit_dir +| normal *| cos_theta) *| refr_ratio in
              let r_out_parallel = normal *| -. sqrt (abs_float (1. -. Vec.mag2 r_out_perp)) in
              r_out_perp +| r_out_parallel
            in
            {pos; dir = refract}
      in
      let color = ray_color ray (count - 1) (hit ray) in
      Vec.{
        x = color.x *. mat_color.x;
        y = color.y *. mat_color.y;
        z = color.z *. mat_color.z;
      }
  in
  ray_color r 20 (hit r)


let write_color array row col (color : vec) num_samples =
  let scale = 1. /. num_samples in
  let color = (Vec.albedo_correct @@ color *| scale) *| 255. in
  let color_of_float float =
    int_of_float float
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

let random_scene () =
    let ground =
      {center = (Vec.make 0. (-1000.) 0.); radius = 1000.; mat = Diffuse (Vec.make 0.5 0.5 0.5)}
    in
    let num = 23 in
    let random =
      List.init num (fun i ->
          List.init num (fun j ->
              let center =
                Vec.make (float_of_int (i - num / 2)) 0.2 (float_of_int (j - num / 2))
                +| Vec.random 0.9 0. 0.9
              in
              if Vec.mag (center -| Vec.make 4. 0.2 0.) < 0.9
              then None
              else
                let mat =
                  let mat_rd = !rng () in
                  if mat_rd < 0.8
                  then Diffuse (Vec.make (!rng () *. !rng ()) (!rng () *. !rng ()) (!rng () *. !rng ()))
                  else if mat_rd < 0.95
                  then FuzzyMetal (Vec.make 0.5 0.5 0.5 +| Vec.random 0.5 0.5 0.5, !rng ())
                  else Dielectric 1.5
                in
                Some {center; radius = 0.2; mat}
            )
        )
      |> List.flatten
      |> List.filter_map (fun x -> x)
    in
    let big_spheres =
      let radius = 1. in
      [
      {center = Vec.make 0. 1. 0.; radius; mat = Dielectric 1.5; };
      {center = Vec.make (-4.) 1. 0.; radius; mat = Diffuse (Vec.make 0.4 0.2 0.1); };
      {center = Vec.make 4. 1. 0.; radius; mat = Metal (Vec.make 0.7 0.6 0.5); };
    ]
    in
    ground :: big_spheres @ random


let main rng_arg array (w_i,h_i) sqrt_samples_per_pixel =
  rng := rng_arg;
  let w, h = float_of_int w_i, float_of_int h_i in
  let cam =
    let lookfrom = Vec.make 13. 2. 3. in
    let lookat = Vec.make 0. 0. 0. in
    let dist_to_focus = Vec.mag (lookfrom -| lookat) in
    let dist_to_focus = 10. in
    let aperture = 0.1 in
    Camera.make
      lookfrom lookat
      Vec.(make 0. 1. 0.)
      20. (w /. h)
      aperture
      dist_to_focus
  in
  let world = [
    {center = (Vec.make (-1.) 0. (-1.)); radius = 0.5; mat = Metal (Vec.make 0.8 0.8 0.8)};
    {center = (Vec.make 0. 0. (-1.)); radius = 0.5; mat = Dielectric 1.5};
    (* {center = (Vec.make 0. 0. (-1.)); radius = 0.5; mat = Diffuse (Vec.make 0.7 0.3 0.3)}; *)
    {center = (Vec.make 1. 0. (-1.)); radius = 0.5; mat = FuzzyMetal (Vec.make 0.8 0.6 0.2, 0.1)};
    (* {center = (Vec.make 0. c (-1.)); radius = (c -. 0.5); color = (Vec.make 1. 0. 0.)}; *)
    (* {center = (Vec.make 0. (-10.5) (-1.)); radius = 9.; color = (Vec.make 0. 1. 0.)}; *)
    {center = (Vec.make 0. (-100.5) (-1.)); radius = 100.; mat = Diffuse (Vec.make 0.8 0.8 0.)};

  ] in
  let world = random_scene () in
  Brr.Console.log ["Raytracing"];
  let samples = Array.init h_i (fun row ->
      Brr.Console.log [row];
      Array.init w_i (fun col ->
          let color = ref (Vec.make 0. 0. 0.) in
          for sample1 = 0 to sqrt_samples_per_pixel - 1 do
            for sample2 = 0 to sqrt_samples_per_pixel - 1 do
              let u = ((float_of_int sample1) *. 0.1 +. (float_of_int col)) /. (w -. 1.) in
              let v = ((float_of_int sample2) *. 0.1 +. (float_of_int row)) /. (h -. 1.) in

              let r = Camera.ray cam u v in

              color := !color +| (ray_color world r);
            done;
          done;
          !color
        )
    )
  in
  Brr.Console.log ["Aggregating samples"];
  let kernel = 0 in
  let kernel_weight i j = match kernel with
    | 1 -> [|
        [|4.;2.|];
        [|2.;1.|];
      |].(abs i).(abs j)
    | 2 -> [|
        [|41.;26.;7.|];
        [|26.;16.;4.|];
        [|07.;04.;1.|];
      |].(abs i).(abs j)
    | 3 -> [|
        [|159.;97.;22.;2.|];
        [|97.;59.;13.;1.|];
        [|22.;13.;3.;0.|];
        [|2.;1.;0.;0.|];
      |].(abs i).(abs j)
    | _ -> 1.
  in
  let kernel_sum = float_of_int @@ match kernel with
    | 1 -> 16
    | 2 -> 273
    | 3 -> 1003
    | _ -> (kernel*2+1) * (kernel*2+1)
  in
  for row = kernel to h_i - kernel - 1 do
    for col = kernel to w_i - kernel - 1 do
      let color = ref (Vec.make 0. 0. 0.) in
      for sample1 = -kernel to kernel do
        for sample2 = -kernel to kernel do
          color := !color +| samples.(row + sample1).(col + sample2) *| kernel_weight sample1 sample2;
        done;
      done;
      samples.(row).(col) <- !color /| kernel_sum;
    done;
  done;
  Brr.Console.log ["Writing file"];
  for row = 0 to h_i - 1 do
    for col = 0 to w_i - 1 do
      (* print_chance 0.0001 (fun () -> Printf.printf "%f %f %f\n" r.dir.x r.dir.y r.dir.z); *)
      let samples_per_pixel = float_of_int (sqrt_samples_per_pixel * sqrt_samples_per_pixel) in
      write_color array row col samples.(row).(col) samples_per_pixel
    done;
  done;

  ()
