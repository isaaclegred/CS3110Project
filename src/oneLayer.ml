open Trainer

module Make (In : Data) (Out : Data) (D : Derivative) = struct

  module M = Owl.Mat
  module In = In
  module Out = Out
  module D = D

  type t = {
    input : float array array;
    output : float array array;
    network : Network.net;
  }

  let learning_rate = 0.03 (* Arbitrarily chosen *)

  let layer =
    Layer.create
      (M.uniform In.size Out.size)
      (M.zeros Out.size 1)
      (List.init Out.size (fun _ -> Owl.Maths.sigmoid))

  let create ins outs =
    let input = Array.map In.to_float_array ins in
    let output = Array.map Out.to_float_array outs in
    let network =
      Network.(
        create In.size Out.size
        |> seal layer
      ) in
    {input; output; network}

  let cost expected actual =
    let to_mat arr = M.of_array arr (Array.length arr) 1 in
    let expected = expected |> Out.to_float_array |> to_mat in
    let actual = actual |> to_mat in
    M.(expected - actual |> sqr |> sum')

  let update {input; output; network} = failwith "Unimplemented"

  let train {input; output; network} = failwith "Unimplemented"

  let get_network {input; output; network} = network

end
