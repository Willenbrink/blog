module Ray = struct
  type t = Gg.p3 * Gg.v3

  let orig = fst
  let dir = snd
  let at t (orig, dir) = orig + t * dir
end



let main array =
  ()
