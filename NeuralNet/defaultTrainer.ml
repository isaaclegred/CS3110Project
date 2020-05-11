module Mat = Owl.Mat
open Trainer

module Make (In : Data) (Out : Data) = struct

  module In = In
  module Out = Out

  type t = {
    input : Mat.mat;
    output : Mat.mat;
    network : Network.net;
    deriv : Network.net -> (Mat.mat * Mat.mat) array;
    rate : float;
  }

  let default_rate : float = 0.0001 (* Arbitrarily chosen *)

  let create (ins : In.t) (outs : Out.t) (network : Network.net) : t =
    let to_mat arr size = arr |> Mat.of_array size 1 |> Mat.transpose in
    let input = ins |> In.to_float_array |> to_mat In.size in
    let output = outs |> Out.to_float_array |> to_mat Out.size in
    let deriv = Derivative.eval input output in
    {input; output; network; deriv; rate = default_rate}

  type update_status =
    | Accept of t
    | Reject of t

  (** [cost t] is the cost of [t]'s neural net relative to the training data. *)
  let cost {input; output; network; deriv; rate} : float =
    cost_of_mat output (Network.run_mat network input)

  let update ({input; output; network; deriv; rate} as t) : update_status =
    let weights, biases =
      network
      |> deriv
      |> Array.map (fun (w, b) -> Mat.(rate $* w, rate $* b))
      |> (fun x -> Array.map fst x, Array.map snd x) in
    let new_network = Network.decr weights biases network in
    let attempt = {input; output; network = new_network; deriv; rate} in
    if cost attempt <= cost t then
      Accept {input; output; network = new_network; deriv; rate = rate *. 1.05}
    else
      Reject {input; output; network; deriv; rate = rate *. 0.2}

  let rec train (trainer : t) (threshold : float) : t =
    if cost trainer < threshold then
      trainer
    else
      match update trainer with
      | Accept new_trainer
      | Reject new_trainer
        -> train new_trainer threshold

  let get_network {input; output; network; deriv; rate} : Network.net =
    network

end
