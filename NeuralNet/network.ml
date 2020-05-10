module Mat = Owl.Mat

type pre_net = {
  input_size : int;
  output_size : int;
  layers : Layer.t list;
}

type net = Layer.t array

let create (input_size : int) (output_size : int) : pre_net =
  if input_size < 0 || output_size < 0 then
    Invalid_argument "Sizes cannot be negative" |> raise
  else
    {input_size; output_size; layers = []}

(** A layer added to a pre_net must agree with all the layers that came before
    it, i.e. the invariant of a pre_net is that all of the weights belonging to
    the layers must be composable into a a valid matrix product that terminates
    with a matrix that returns [output_size] floats. No guarantee is made as to
    what forms a valid input to the matrix product. *)
let add_layer (layer : Layer.t) {input_size; output_size; layers} : pre_net =
  let size =
    match layers with
    | [] -> input_size
    | h :: t -> Layer.layer_size h in
  if Layer.input_size layer <> size then
    Invalid_argument "Layer has bad input shape" |> raise
  else {input_size; output_size; layers = layer :: layers}

let seal ({input_size; output_size; layers} : pre_net) : net =
  let size =
    match layers with
    | [] -> failwith "This is impossible!"
    | h :: t -> Layer.layer_size h in
  if size <> output_size then
    Invalid_argument "Shapes do not match" |> raise
  else
    layers
    |> List.rev
    |> Array.of_list

let input_size (network : net) : int =
  Layer.input_size network.(0)

let output_size (network : net) : int =
  Layer.layer_size network.(Array.length network - 1)

let run (network : net) (inputs : float array) : float array =
  inputs
  |> (fun arr -> Mat.of_array arr (Array.length arr) 1)
  |> (fun mat -> Array.fold_left Layer.run mat network)
  |> Mat.to_array

let run_mat (network : net) (inputs : Mat.mat) : Mat.mat =
  Array.fold_left Layer.run inputs network

let prop f g weights biases network =
  let size = Array.length network in
  if Array.length weights <> size || Array.length biases <> size then
    Invalid_argument "Invalid sizes" |> raise
  else
    Array.mapi (fun i x -> x |> f weights.(i) |> g biases.(i)) network

let update : Mat.mat array -> Mat.mat array -> net -> net =
  prop Layer.set_weights Layer.set_biases

let incr : Mat.mat array -> Mat.mat array -> net -> net =
  prop Layer.incr_weights Layer.incr_biases

let decr : Mat.mat array -> Mat.mat array -> net -> net =
  prop Layer.decr_weights Layer.decr_biases

let pre_net_layers {input_size; output_size; layers} : Layer.t array =
  layers |> List.rev |> Array.of_list

let net_layers : net -> Layer.t array =
  Array.copy

let copy_pre_net {input_size; output_size; layers} : pre_net =
  {input_size; output_size; layers = List.map Layer.copy layers}

let copy_net : net -> net =
  Array.map Layer.copy

let print_pre_net {input_size; output_size; layers} : unit =
  print_string "Input size: "; print_int input_size;
  print_string "Output size: "; print_int output_size;
  print_endline "Layers:";
  List.iter Layer.print layers

let print_net : net -> unit =
  Array.iter Layer.print

let to_parameter_list (net : net) : (Mat.mat * Mat.mat) list =
  let f lst layer = (Layer.get_weights layer, Layer.get_biases layer) :: lst in
  net
  |> Array.fold_left f []
  |> List.rev

let from_parameter_list (params : (Mat.mat * Mat.mat) list) : net =
  let params = Array.of_list params in
  let act x = x in
  let act_deriv _ = 1. in
  let make b f = List.init (Mat.row_num b) (fun _ -> f) in
  let f (w, b) = Layer.create w b (make b act) (make b act_deriv) in
  Array.map f params
