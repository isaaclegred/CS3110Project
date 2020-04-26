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
    M.((to_mat expected) - (to_mat actual) |> sqr |> sum')

  let update {input; output; network} =
    let layers = Network.net_layers network in
    let weights = Array.map Layer.get_weights layers in
    let biases = Array.map Layer.get_biases layers in
    let delta = D.eval weights biases in
    {input; output; network = Network.incr (fst delta) (snd delta) network}

  let train {input; output; network} = failwith "Unimplemented"

  let get_network {input; output; network} = network

end
