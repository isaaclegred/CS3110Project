open Trainer

module Make (In : Data) (Out : Data) = struct

  module M = Owl.Mat
  module In = In
  module Out = Out

  type t = unit

  let layer =
    Layer.create
      (M.uniform In.size Out.size)
      (M.zeros Out.size 1)
      (List.init Out.size (fun _ -> Owl.Maths.sigmoid))

  let create ins outs =
    Network.(
      create In.size Out.size
      |> seal layer
    )

  let cost expected actual =
    let to_mat arr = M.of_array arr (Array.length arr) 1 in
    let expected = expected |> Out.to_float_array |> to_mat in
    let actual = actual |> to_mat in
    M.(expected - actual |> sqr |> sum')

end
