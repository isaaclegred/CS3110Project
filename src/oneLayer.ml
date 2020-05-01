open Trainer

module Make (In : Data) (Out : Data)
    (D : Derivative with module In = In and module Out = Out) = struct

  module M = Owl.Mat
  module In = In
  module Out = Out
  module D = D

  type t = {
    input : M.mat;
    output : M.mat;
    network : Network.net;
    deriv :
      Network.net ->
      M.mat array -> M.mat array ->
      M.mat array * M.mat array
  }

  let learning_rate = ref 0.00001 (* Arbitrarily chosen *)

  let layer =
    Layer.create
      (M.uniform Out.size In.size)
      (M.zeros Out.size 1)
      (List.init Out.size (fun _ -> (* TODO Owl.Maths.sigmoid *) fun x -> x))

  let create ins outs =
    if Array.length ins <> Array.length outs then
      Invalid_argument "Inputs and outputs must be the same length" |> raise
    else
      let to_mat arrs = arrs |> M.of_arrays |> M.transpose in
      let input = ins |> Array.map In.to_float_array |> to_mat in
      let output = outs |> Array.map Out.to_float_array |> to_mat in
      let network =
        Network.(
          create In.size Out.size
          |> seal layer
        ) in
      {input; output; network; deriv = D.eval input output}

  type update_status =
    | Accept of t
    | Reject of t
  let update {input; output; network; deriv} =
    let original_cost = cost (Network.run network (M.to_array input))
         (M.to_array output) in 
    let f = !learning_rate |> M.($*) |> Array.map in
    let layers = network |> Network.net_layers in
    let ws = layers |> Array.map Layer.get_weights in
    let bs = layers |> Array.map Layer.get_biases in
    let weights, biases = deriv network ws bs |> fun (x, y) -> f x, f y in
    let new_network = Network.decr weights biases network in
    let attempt = {input; output;
                   network = new_network; deriv} in
    let final_cost = cost (Network.run network (M.to_array input))
        (M.to_array output) in
    if (final_cost > original_cost) then Reject {input; output; network; deriv}
    else Accept attempt

  let train {input; output; network; deriv} = failwith "Unimplemented"

  let get_network {input; output; network; deriv} = network

end
