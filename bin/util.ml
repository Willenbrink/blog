open Brr

let log x =
  Console.log [x];
  x

let button ?at onclick label =
  let but = El.button ?at [El.txt (Jstr.v label)] in
  ignore (Ev.listen Ev.click (fun _e -> onclick ()) (El.as_target but)); but
