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
    if Array.length ins <> Array.length outs then
      Invalid_argument "Inputs and outputs must be the same length" |> raise
    else
      let input = Array.map In.to_float_array ins in
      let output = Array.map Out.to_float_array outs in
      let network =
        Network.(
          create In.size Out.size
          |> seal layer
        ) in
      {input; output; network}

  let update {input; output; network} =
    let layers = Network.net_layers network in
    let weights = Array.map Layer.get_weights layers in
    let biases = Array.map Layer.get_biases layers in
    let delta = D.eval weights biases in
    {input; output; network = Network.decr (fst delta) (snd delta) network}

  let train {input; output; network} = failwith "Unimplemented"

  let get_network {input; output; network} = network

end
