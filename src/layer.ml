module M = Owl.Mat

type t = {
  weights : M.mat;
  biases : M.mat;
  activations : int -> float -> float;
}

let create weights biases acts =
  let size = List.length acts in
  if M.row_num weights <> size || M.shape biases <> (size, 1) then
    Invalid_argument "Shapes do not match" |> raise
  else {weights; biases; activations = List.nth acts}

let input_size layer = M.col_num layer.weights

let layer_size layer = M.row_num layer.weights

let get_weights layer = M.copy layer.weights

let set_weights new_weights ({weights; biases; activations} as layer) =
  if M.row_num new_weights <> layer_size layer then
    Invalid_argument "Bad shape" |> raise
  else {weights = M.copy new_weights; biases; activations}

let incr_weights new_weights ({weights; biases; activations} as layer) =
  if M.row_num new_weights <> layer_size layer then
    Invalid_argument "Bad shape" |> raise
  else {weights = M.(weights + new_weights); biases; activations}

let get_biases layer = M.copy layer.biases

let set_biases new_biases {weights; biases; activations} =
  if M.shape new_biases <> M.shape biases then
    Invalid_argument "Bad shape" |> raise
  else {weights; biases = M.copy new_biases; activations}

let incr_biases new_biases {weights; biases; activations} =
  if M.shape new_biases <> M.shape biases then
    Invalid_argument "Bad shape" |> raise
  else {weights; biases = M.(biases + new_biases); activations}

let get_activations layer = List.init (layer_size layer) layer.activations

let set_activations new_activations ({weights; biases; activations} as layer) =
  if List.length new_activations <> layer_size layer then
    Invalid_argument "Bad length" |> raise
  else {weights; biases; activations = List.nth new_activations}

let run input ({weights; biases; activations} as layer) =
  if M.shape input <> (input_size layer, 1) then
    Invalid_argument "Bad shape" |> raise
  else M.(weights *@ input + biases |> mapi activations)

let to_string layer = failwith "Unimplemented" (* TODO *)

let from_string data = failwith "Unimplemented" (* TODO *)

let copy {weights; biases; activations} =
  {weights = M.copy weights; biases = M.copy biases; activations}

let print {weights; biases; activations} =
  print_endline "Weights:";
  M.print weights;
  print_endline "Biases:";
  M.print biases
