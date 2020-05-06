module M = Owl.Mat

type t = {
  weights : M.mat;
  biases : M.mat;
  activations : int -> float -> float;
  activation_derivative : int -> float -> float 
}

let create weights biases acts act_derivs=
  let size = List.length acts in
  if M.col_num weights <> size || M.shape biases <> (size, 1) then
    Invalid_argument "Shapes do not match" |> raise
  else {weights; biases; activations = List.nth acts;
        activation_derivative = List.nth act_derivs}

let input_size layer = M.col_num layer.weights

let layer_size layer = M.row_num layer.weights

let get_weights layer = M.copy layer.weights

let change_weights f new_weights ({weights; biases; activations; activation_derivative}
                                  as layer) =
  if M.row_num new_weights <> layer_size layer then
    Invalid_argument "Bad shape" |> raise
  else {weights = f weights new_weights; biases; activations; activation_derivative}

let set_weights = change_weights (fun _ m -> M.copy m)

let incr_weights = change_weights M.(+)

let decr_weights = change_weights M.(-)

let get_biases layer = M.copy layer.biases

let change_biases f new_biases {weights; biases; activations; activation_derivative} =
  if M.shape new_biases <> M.shape biases then
    Invalid_argument "Bad shape" |> raise
  else {weights; biases = f biases new_biases; activations; activation_derivative}

let set_biases = change_biases (fun _ m -> M.copy m)

let incr_biases = change_biases M.(+)

let decr_biases = change_biases M.(-)

let get_activations layer = List.init (layer_size layer) layer.activations

let set_activations new_activations new_derivs
    ({weights; biases; activations; activation_derivative} as layer) =
  if List.length new_activations <> layer_size layer then
    Invalid_argument "Bad length" |> raise
  else {weights; biases; activations = List.nth new_activations;
        activation_derivative = List.nth new_derivs}

let run input ({weights; biases; activations; activation_derivative} as layer) =
  if M.shape input <> (input_size layer, 1) then
    Invalid_argument "Bad shape" |> raise
  else M.(weights *@ ((input + biases) |> mapi activations))

let to_string layer = failwith "Unimplemented" (* TODO *)

let from_string data = failwith "Unimplemented" (* TODO *)

let copy {weights; biases; activations; activation_derivative} =
  {weights = M.copy weights; biases = M.copy biases; activations; activation_derivative}

let print {weights; biases; activations} =
  print_endline "Weights:";
  M.print weights |> Format.print_newline;
  print_endline "Biases:";
  M.print biases |> Format.print_newline


(* Ok so I was wrong, we need both a linearization function which returns the local 
   response of the layer with respect to [input] and this is what we propogate through
   in the process of computing a derivative.  The linearization of the function that adds 
   the biases is the identity matrix, and the function that multiplies by the weights is
   just the matrix of weights
*)


let linearization input {weights; biases; activations; activation_derivative} =
  let input_size = fst (M.shape input) in 
  let act_input = M.(input + biases) in
  let input_deriv = ref (M.copy weights) in
  let rec loop index =
    match index with
    | max when max = input_size -> ()
    | i -> let act_deriv = activation_derivative i M.(get act_input i 1 ) in
      input_deriv := M.(map_at_col (fun elt -> act_deriv *. elt)  !input_deriv i ); loop (index + 1) in
   let _ = loop 0 in !input_deriv

(* The prefactor tells us how the global outputs respond to the local outputs linearly
   the input is the computed input into this layer, since
 *)


let deriv prefactor desired_output actual_output input
    ({weights; biases; activations; activation_derivative} as layer) =
  let out_size = layer_size layer in 
  let residuals = M.(actual_output - desired_output) in
  let local_linearization = linearization input layer in
  let total_linearization = M.(prefactor *@ local_linearization) in 
  let bias_derivs = M.(transpose (residuals) *@ total_linearization) in
  if  snd (M.shape prefactor) <> out_size
  then Invalid_argument "Bad shape in deriv"|> raise  
  else let prefactor_row_sums = 
         M.(prefactor *@ (ones out_size 1))
    in let pre_weight_deriv =  M.(prefactor_row_sums * input) in 
    let weight_deriv = M.(mapi_2d (fun i j elt -> (get residuals i 1) *. elt) pre_weight_deriv) in
    ((weight_deriv, bias_derivs), total_linearization)
    
  
