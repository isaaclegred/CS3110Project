module M = Owl.Mat

type pre_net = {
  input_size : int;
  output_size : int;
  layers : Layer.t list;
}

type net = Layer.t array

let create input_size output_size = {input_size; output_size; layers = []}

let add_layer layer {input_size; output_size; layers} =
  let size =
    match layers with
    | [] -> input_size
    | h :: t -> Layer.layer_size h in
  if Layer.input_size layer <> size then
    Invalid_argument "Layer has bad input shape" |> raise
  else {input_size; output_size; layers = layer :: layers}

let seal layer pre_network =
  let {input_size; output_size; layers} = add_layer pre_network layer in
  let size =
    match layers with
    | [] -> failwith "This is impossible!"
    | h :: t -> Layer.layer_size h in
  if size <> output_size then
    Invalid_argument "Layer has bad input shape" |> raise
  else layers |> List.rev |> Array.of_list

let input_size network = Layer.input_size network.(0)

let output_size network = Layer.layer_size network.(Array.length network - 1)

let run network inputs =
  inputs
  |> (fun arr -> M.of_array arr (Array.length arr) 1)
  |> (fun mat -> Array.fold_left Layer.run mat network)
  |> M.to_array

let to_string network = failwith "Unimplemented" (* TODO *)

let from_string data = failwith "Unimplemented" (* TODO *)

let copy_pre_net {input_size; output_size; layers} =
  {input_size; output_size; layers = List.map Layer.copy layers}

let copy_net = Array.map Layer.copy

let print_pre_net {input_size; output_size; layers} =
  print_string "Input size: "; print_int input_size;
  print_string "Output size: "; print_int output_size;
  print_endline "Layers:";
  List.iter Layer.print layers

let print_net = Array.iter Layer.print
