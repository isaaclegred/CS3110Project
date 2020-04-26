module type Data = sig
  type t
  val to_float : t -> float
  val from_float : float -> t
end

module type Network = sig
  module D : Data
  type pre_net
  type net
  val create : int -> int -> pre_net
  val add_layer : pre_net -> Layer.t -> pre_net
  val seal : pre_net -> Layer.t -> net
  val input_size : net -> int
  val output_size : net -> int
  val run : net -> D.t list -> D.t list
  val to_string : net -> string
  val from_string : string -> net
  val copy : net -> net
end

module Make (D : Data) = struct

  module M = Owl.Mat

  module D = D

  type pre_net = {
    input_size : int;
    output_size : int;
    layers : Layer.t list;
  }

  type net = Layer.t array

  let create input_size output_size = {input_size; output_size; layers = []}

  let add_layer {input_size; output_size; layers} layer =
    let size =
      match layers with
      | [] -> input_size
      | h :: t -> Layer.layer_size h in
    if size <> Layer.input_size layer then
      Invalid_argument "Layer has bad input shape" |> raise
    else {input_size; output_size; layers = layer :: layers}

  let seal pre_network layer =
    let {input_size; output_size; layers} = add_layer pre_network layer in
    let size =
      match layers with
      | [] -> failwith "This is impossible!"
      | h :: t -> Layer.layer_size h in
    if output_size <> size then
      Invalid_argument "Layer has bad input shape" |> raise
    else layers |> List.rev |> Array.of_list

  let input_size network = Layer.input_size network.(0)

  let output_size network = Layer.layer_size network.(Array.length network - 1)

  let run network inputs =
    inputs
    |> Array.of_list
    |> Array.map D.to_float
    |> (fun arr -> M.of_array arr (Array.length arr) 1)
    |> (fun mat -> Array.fold_left Layer.run mat network)
    |> M.to_array
    |> Array.map D.from_float
    |> Array.to_list

  let to_string network = failwith "Unimplemented" (* TODO *)

  let from_string data = failwith "Unimplemented" (* TODO *)

  let copy = Array.map Layer.copy

end
