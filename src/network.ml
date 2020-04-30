module M = Owl.Mat

type pre_net = {
  input_size : int;
  output_size : int;
  layers : Layer.t list;
}

type net = Layer.t array

let create input_size output_size = {input_size; output_size; layers = []}

(* A layer added to a pre_net must agree with all the layers that came before it,
   i.e. the invariant of a pre_net is that all of the weights beloning to the layers
   must be composable into a a valid matrix product that terminates with a matrix that 
   returns [output_size] floats.  No guarantee is made of what is a valid input to 
   the matrix product. 
*)
let add_layer layer {input_size; output_size; layers} =
  let size =
    match layers with
    | [] -> input_size
    | h :: t -> Layer.layer_size h in
  if Layer.input_size layer <> size then
    Invalid_argument "Layer has bad input shape" |> raise
  else {input_size; output_size; layers = layer :: layers}


(* Add the final layer to the neural net, the resulting map must take [input_size]
   to [output_size] floats.  A network can be seen as a pre_net with an additional
   condition that the matrix product of weights takes arguments of size [input_size]
*)
let seal layer pre_network =
  let {input_size; output_size; layers} = add_layer layer pre_network in
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

let prop f g weights biases network =
  let size = Array.length network in
  if Array.length weights <> size || Array.length biases <> size then
    Invalid_argument "Invalid sizes" |> raise
  else Array.mapi (fun i x -> x |> f weights.(i) |> g biases.(i)) network

let update = prop Layer.set_weights Layer.set_biases

let incr = prop Layer.incr_weights Layer.incr_biases

let decr = prop Layer.decr_weights Layer.decr_biases

let to_string network = failwith "Unimplemented" (* TODO *)

let from_string data = failwith "Unimplemented" (* TODO *)

let pre_net_layers {input_size; output_size; layers} =
  layers |> List.rev |> Array.of_list

let net_layers = Array.copy

let copy_pre_net {input_size; output_size; layers} =
  {input_size; output_size; layers = List.map Layer.copy layers}

let copy_net = Array.map Layer.copy

let print_pre_net {input_size; output_size; layers} =
  print_string "Input size: "; print_int input_size;
  print_string "Output size: "; print_int output_size;
  print_endline "Layers:";
  List.iter Layer.print layers

let print_net = Array.iter Layer.print
