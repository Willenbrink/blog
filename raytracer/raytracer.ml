let print_chance chance printf =
  if Random.float 1. < chance
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

module Sphere = struct
  type t = { center: Vec.t; radius: float; color: Vec.t }

  let hit {center; radius; _} {pos; dir} t_min t_max =
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

  let hit_sphere {center; radius; _} {pos; dir} =
    let pc = pos -| center in
    let a = Vec.dot dir dir in
    let b = Vec.dot pc dir *. 2. in
    let c = Vec.dot pc pc -. radius *. radius in
    let discr = b*.b -. 4.*.a*.c in
    if discr > 0.
    then Some (( (-. b) -. sqrt discr) /. (2. *. a))
    else None


end

type sphere = Sphere.t = { center: Vec.t; radius: float; color: Vec.t }

let normal ({center; radius; _}) r dist =
  Vec.normalize ((Ray.at dist r) -| center)

let hits r world =
  let f acc el =
    let max_dist = match acc with
      | None -> Float.infinity
      | Some (_,dist) -> dist
    in
    match Sphere.hit el r 0. max_dist with
    | None -> acc
    | Some hit -> Some (el, hit)
  in
  List.fold_left f None world

let color ({pos; dir} as r) = function
  | None ->
    let unit = Vec.normalize dir in
    let t = 0.5 *. (unit.y +. 1.) in
    Vec.(make 1. 1. 1.) *| (1. -. t) +| Vec.(make 0.5 0.7 1.) *| t
  | Some (el,dist) ->
    (normal el r dist +| Vec.(make 1. 1. 1.)) *| 0.5


let write_color array row col (color : vec) =
  let r = int_of_float (color.x *. 255.) in
  let g = int_of_float (color.y *. 255.) in
  let b = int_of_float (color.z *. 255.) in
  begin
    try
      Bigarray.Array2.set array row (4*col+0) r;
      Bigarray.Array2.set array row (4*col+1) g;
      Bigarray.Array2.set array row (4*col+2) b;
      ()
    with e -> Printf.printf "col %i(%i), row %i" col (4*col) row; raise e
  end


let main array (w_i,h_i) =
  let w, h = float_of_int w_i, float_of_int h_i in
  let h_vp = 2. in
  let w_vp = w /. h *. h_vp in
  let focal_length = 1. in

  let origin = Vec.make 0. 0. 0. in
  let horizontal = Vec.make w_vp 0. 0. in
  let vertical = Vec.make 0. h_vp 0. in
  let ll_corner = origin -| horizontal /| 2. -| vertical /| 2. -| (Vec.make 0. 0. focal_length) in
  let world = [
    {center = (Vec.make 0. 0. (-1.)); radius = 0.5; color = (Vec.make 1. 0. 0.)};
    (* {center = (Vec.make 0. c (-1.)); radius = (c -. 0.5); color = (Vec.make 1. 0. 0.)}; *)
    (* {center = (Vec.make 0. (-10.5) (-1.)); radius = 9.; color = (Vec.make 0. 1. 0.)}; *)
    {center = (Vec.make 0. (100.5) (-1.)); radius = 100.; color = (Vec.make 0. 1. 0.)};

  ] in
  for row = 0 to h_i - 1 do
    for col = 0 to w_i - 1 do
      let u = (float_of_int col) /. (w -. 1.) in
      let v = (float_of_int row) /. (h -. 1.) in
      let r = {
        pos = origin;
        dir = Vec.normalize @@ ll_corner  +| horizontal *| u +| vertical *| v -| origin
      }
      in

      let color =
        hits r world
        |> color r
      in

      (* print_chance 0.0001 (fun () -> Printf.printf "%f %f %f\n" r.dir.x r.dir.y r.dir.z); *)
      write_color array row col color

    done
  done;



  ()
