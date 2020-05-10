module Mat = Owl.Mat

type t = {
  weights : Mat.mat;
  biases : Mat.mat;
  activations : int -> float -> float;
  activation_derivative : int -> float -> float;
}

let create
    (weights : Mat.mat) (biases : Mat.mat)
    (acts : (float -> float) list) (act_derivs : (float -> float) list)
  : t =
  let size = List.length acts in
  let invalid =
    List.length act_derivs <> size ||
    Mat.col_num weights <> size ||
    Mat.shape biases <> (size, 1) in
  if invalid then
    Invalid_argument "Shapes do not match" |> raise
  else
    let acts = Array.of_list acts in
    let act_derivs = Array.of_list act_derivs in
    {
      weights = Mat.copy weights;
      biases = Mat.copy biases;
      activations = Array.get acts;
      activation_derivative = Array.get act_derivs;
    }

let input_size (layer : t) : int =
  Mat.col_num layer.weights

let layer_size (layer : t) : int =
  Mat.row_num layer.weights

let get_weights (layer : t) : Mat.mat =
  Mat.copy layer.weights

(** [change_weights f new_weights layer] is [layer] except with weights set to
    [f weights new_weights].

    Raises: [Invalid_argument] if [f weights new_weights] differs in shape from
    [get_weights layer]. *)
let change_weights
    (f : Mat.mat -> Mat.mat -> Mat.mat)
    (new_weights : Mat.mat)
    ({weights; biases; activations; activation_derivative} as layer : t)
  : t =
  let weights = f weights new_weights in
  if Mat.shape weights <> (layer_size layer, input_size layer) then
    Invalid_argument "Bad shape weights" |> raise
  else
    {weights; biases; activations; activation_derivative}

let set_weights : Mat.mat -> t -> t =
  change_weights (fun _ -> Mat.copy)

let incr_weights : Mat.mat -> t -> t =
  change_weights Mat.(+)

let decr_weights : Mat.mat -> t -> t =
  change_weights Mat.(-)

let get_biases (layer : t) : Mat.mat =
  Mat.copy layer.biases

let change_biases
    (f : Mat.mat -> Mat.mat -> Mat.mat)
    (new_biases : Mat.mat)
    ({weights; biases; activations; activation_derivative} as layer : t)
  : t =
  let biases = f biases new_biases in
  if Mat.shape biases <> (input_size layer, 1) then
    Invalid_argument "Bad shape biases" |> raise
  else {weights; biases; activations; activation_derivative}

let set_biases : Mat.mat -> t -> t =
  change_biases (fun _ -> Mat.copy)

let incr_biases : Mat.mat -> t -> t =
  change_biases Mat.(+)

let decr_biases : Mat.mat -> t -> t =
  change_biases Mat.(-)

let get_activations (layer : t) : (float -> float) list =
  List.init (input_size layer) layer.activations

let set_activations
    (new_acts : (float -> float) list)
    (new_derivs : (float -> float) list)
    ({weights; biases; activations; activation_derivative} : t)
  : t =
  create weights biases new_acts new_derivs

let run_with_intermediate
    (input : Mat.mat)
    ({weights; biases; activations; activation_derivative} as layer : t)
  : Mat.mat * Mat.mat =
  if Mat.shape input <> (input_size layer, 1) then
    Invalid_argument "Bad shape" |> raise
  else
    let partial_result = Mat.(input + biases |> mapi activations) in
    partial_result, Mat.(weights *@ partial_result)

let run (input : Mat.mat) (layer : t) : Mat.mat =
  layer
  |> run_with_intermediate input
  |> snd

let copy ({weights; biases; activations; activation_derivative} : t) : t =
  {
    weights = Mat.copy weights;
    biases = Mat.copy biases;
    activations;
    activation_derivative;
  }

let print ({weights; biases; activations; activation_derivative} : t) : unit =
  print_endline "Weights:";
  Mat.print weights |> Format.print_newline;
  print_endline "Biases:";
  Mat.print biases |> Format.print_newline

(** [linearization layer] is the linearization of [layer].

    The linearization of the function that adds the biases is the identity
    matrix, and the linearization of the function that multiplies the weights
    is just the matrix of weights. *)
let linearization
    ({weights; biases; activations; activation_derivative} : t)
    (input : Mat.mat)
  : Mat.mat =
  let input = Mat.(input + biases) |> Mat.to_array in
  let act_deriv i = activation_derivative i input.(i) in
  Mat.(mapi_2d (fun i j x -> act_deriv j *. x) weights)

let deriv
    (prefactor : Mat.mat)
    (desired_output : Mat.mat)
    (actual_output : Mat.mat)
    (partial_input, input : Mat.mat * Mat.mat)
    (layer : t)
  : (Mat.mat * Mat.mat) * Mat.mat =
  let residuals = Mat.(actual_output - desired_output) in
  let linearized = Mat.(prefactor *@ (linearization layer input)) in
  let weight_deriv =
    residuals
    |> (fun x -> Mat.(transpose prefactor *@ x))
    |> (fun x -> Mat.(x *@ transpose partial_input)) in
  let bias_deriv =
    residuals
    |> Mat.( *@ ) Mat.(transpose linearized) in
  (weight_deriv, bias_deriv), linearized
