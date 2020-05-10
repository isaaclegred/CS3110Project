module Mat = Owl.Mat

(** [eval_layers input layers] is an array [values] of length
    [Array.length layers + 1] that satisfies the following conditions:
    - [values.(0) = (input, input)], and
    - for [1 <= i < Array.length layers + 1], [values.(i) =
      Layer.run_with_intermediate (snd values.(i - 1)) layers.(i - 1)].

    Note that [values] does not share mutable elements with the inputs, so it
    is safe to modify anything you want after the call.

    Requires:
    - [Mat.col_num input = 1],
    - if [Array.length layers <> 0] then
      [Mat.row_num input = Layer.input_size layers.(0)], and
    - [layers] came from [Network.pre_net_layers] or [Network.net_layers]. *)
let eval_layers
    (input : Mat.mat) (layers : Layer.t array)
  : (Mat.mat * Mat.mat) array =
  let num_layers = Array.length layers in
  let values = Array.make (num_layers * 2 + 2) (Mat.copy input) in
  for i = 0 to num_layers - 1 do
    let x, y = Layer.run_with_intermediate values.(i * 2) layers.(i) in
    values.(i * 2 + 1) <- x;
    values.(i * 2 + 2) <- y
  done;
  values.(num_layers * 2 + 1) <- values.(num_layers * 2);
  Array.init (num_layers + 1) (fun i -> values.(i * 2), values.(i * 2 + 1))

(** [eval_derivative input desired_output layers] is an array [derivs] of
    length [Array.length layers], so that
    [derivs.(i) = ((weight_deriv, bias_deriv), linearization)], where
    [weight_deriv] is the derivative of the weights at layer [i],
    [bias_deriv] is the derivative of the biases at layer [i], and
    [linearization] is the linearization of layer [i].

    In greater detail:

    The basic principle of the neural net is that each layer is its own
    function from inputs to outputs and when streamed together, become one
    large function [input -> input_1 -> ... -> input_n -> predicted_output],
    where each [->] represents a layer.

    In order to adjust the model via training, it is necessary to know how
    [predicted_output] responds to changes in each of the layers.
    [weight_deriv] is the derivative of the cost function with respect to the
    weights of the layer,
    [bias_deriv] is the derivative of the cost function with respect to the
    biases of the layer, and
    [linearization] is the derivative of [input_i] with respect to
    [input_{i-1}] for layer [i].

    Requires:
    - if [Array.length layers <> 0] then [Mat.col_num input = 1],
    - if [n = Array.length layers] and [n <> 0] then
      [Mat.row_num input = Layer.input_size layers.(0)] and
      [Mat.row_num output = Layer.layer_size layers.(n - 1)], and
    - [layers] came from [Network.pre_net_layers] or [Network.net_layers]. *)
let eval_derivative
    (input : Mat.mat) (desired_output : Mat.mat) (layers : Layer.t array)
  : ((Mat.mat * Mat.mat) * Mat.mat) array =
  let num_layers = Array.length layers in
  if num_layers = 0 then
    [||]
  else
    let values = eval_layers input layers in
    let true_output = snd values.(num_layers) in
    let deriv i prefactor =
      Layer.deriv prefactor desired_output true_output values.(i) layers.(i) in
    let derivs =
      desired_output
      |> Mat.row_num
      |> Mat.eye
      |> deriv (num_layers - 1)
      |> Array.make num_layers in
    for i = num_layers - 2 downto 0 do
      derivs.(i) <- deriv i (snd derivs.(i + 1))
    done;
    derivs

let eval
    (input : Mat.mat) (output : Mat.mat) (network : Network.net)
  : (Mat.mat * Mat.mat) array =
  network
  |> Network.net_layers
  |> eval_derivative input output
  |> Array.map fst
