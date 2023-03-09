include Vector3
open Vector3

module Syntax = struct
  type vec = t = {x : float; y : float; z : float}
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
  let open Util in
  { x = a *. rng (); y = b *. rng (); z = c *. rng (); }
