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

type sphere = { center: Vec.t; radius: float; color: Vec.t }

let hit_sphere {center; radius; _} {pos; dir} =
  let pc = pos -| center in
  let a = Vec.dot dir dir in
  let b = Vec.dot pc dir *. 2. in
  let c = Vec.dot pc pc -. radius *. radius in
  let discr = b*.b -. 4.*.a*.c in
  discr > 0.

let ray_color ({pos; dir} as r) =
  let sphere = {center = (Vec.make 0. 0. (-1.)); radius = 0.5; color = (Vec.make 1. 0. 0.)} in
  if hit_sphere sphere r
  then sphere.color
  else
    let unit = Vec.normalize dir in
    let t = 0.5 *. (unit.y +. 1.) in
    Vec.(make 1. 1. 1.) *| (1. -. t) +| Vec.(make 0.5 0.7 1.) *| t


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
  for row = 0 to h_i - 1 do
    for col = 0 to w_i - 1 do
      let u = (float_of_int col) /. (w -. 1.) in
      let v = (float_of_int row) /. (h -. 1.) in
      let r = { pos=origin; dir=ll_corner  +| horizontal *| u +| vertical *| v -| origin } in
      let color = ray_color r in

      (* print_chance 0.0001 (fun () -> Printf.printf "%f %f %f\n" r.dir.x r.dir.y r.dir.z); *)
      write_color array row col color

    done
  done;



  ()
