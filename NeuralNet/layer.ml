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
      weights;
      biases;
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
  List.init (layer_size layer) layer.activations

let set_activations new_activations new_derivs
    ({weights; biases; activations; activation_derivative} as layer) =
  if List.length new_activations <> layer_size layer then
    Invalid_argument "Bad length" |> raise
  else {weights; biases; activations = List.nth new_activations;
        activation_derivative = List.nth new_derivs}

let run input ({weights; biases; activations; activation_derivative} as layer) =
  if Mat.shape input <> (input_size layer, 1) then
    Invalid_argument "Bad shape" |> raise
  else
    let result = Mat.((input + biases) |> mapi activations) in
    Mat.(weights *@ result)

let run_with_intermediate input ({weights; biases; activations; activation_derivative} as layer) =
  if Mat.shape input <> (input_size layer, 1) then
    Invalid_argument "Bad shape" |> raise
  else
    let partial_result = Mat.((input + biases) |> mapi activations) in
    (partial_result, Mat.(weights *@ partial_result))

let to_string layer = failwith "Unimplemented" (* TODO *)

let from_string data = failwith "Unimplemented" (* TODO *)

let copy {weights; biases; activations; activation_derivative} =
  {weights = Mat.copy weights; biases = Mat.copy biases; activations; activation_derivative}

let print {weights; biases; activations; activation_derivative} =
  print_endline "Weights:";
  Mat.print weights |> Format.print_newline;
  print_endline "Biases:";
  Mat.print biases |> Format.print_newline

(* Ok so I was wrong, we need both a linearization function which returns the local
   response of the layer with respect to [input] and this is what we propogate through
   in the process of computing a derivative.  The linearization of the function that adds
   the biases is the identity matrix, and the function that multiplies by the weights is
   just the matrix of weights
*)
let linearization input {weights; biases; activations; activation_derivative} =
  let input_size = fst (Mat.shape input) in
  let act_input = Mat.(input + biases) in
  let input_deriv = ref (Mat.copy weights) in
  let rec loop index =
    match index with
    | max when max = (input_size) -> ()
    | i ->
      let act_deriv = activation_derivative i Mat.(get act_input i 0) in
      input_deriv := Mat.(map_at_col (fun elt -> act_deriv *. elt)  !input_deriv i ); loop (index + 1) in
  let _ = loop 0 in !input_deriv

(* The prefactor tells us how the global outputs respond to the local outputs linearly
   the input is the computed input into this layer, since
*)
let deriv prefactor desired_output actual_output (partial_input, input)
    ({weights; biases; activations; activation_derivative} as layer) =
  let out_size = layer_size layer in
  let residuals = Mat.(actual_output - desired_output) in
  let local_linearization = linearization input layer in
  let total_linearization = Mat.(prefactor *@ local_linearization) in
  let bias_derivs = Mat.(transpose(total_linearization) *@ residuals ) in
  if  snd (Mat.shape prefactor) <> out_size
  then Invalid_argument "Bad shape in deriv"|> raise
  else let weighted_residuals =
         Mat.(transpose(residuals) *@ prefactor)
    in
    let weight_deriv = Mat.(weighted_residuals * partial_input) in
    ((Mat.transpose(weight_deriv), bias_derivs), total_linearization)
