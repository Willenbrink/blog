open Vec.Syntax

let apply array w h kernel =
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
  for row = kernel to h - kernel - 1 do
    for col = kernel to w - kernel - 1 do
      let color = ref (Vec.make 0. 0. 0.) in
      for sample1 = -kernel to kernel do
        for sample2 = -kernel to kernel do
          color := !color +| array.(row + sample1).(col + sample2) *| kernel_weight sample1 sample2;
        done;
      done;
      array.(row).(col) <- !color /| kernel_sum;
    done;
  done;
