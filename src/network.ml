module type Data = sig
  type t
  val to_float : t -> float
  val from_float : float -> t
end

module type Network = sig
  module D : Data
  type t
  val create : int -> int -> t
  val input_size : t -> int
  val layer_size : t -> int
  val get_weights : t -> Owl.Mat.mat
  val set_weights : t -> Owl.Mat.mat -> t
  val get_biases : t -> Owl.Mat.mat
  val set_biases : t -> Owl.Mat.mat -> t
  val get_activations : t -> (float -> float) list
  val set_activations : t -> (float -> float) list -> t
  val run : t -> Owl.Mat.mat -> Owl.Mat.mat
  val to_string : t -> string
  val from_string : string -> t
  val copy : t -> t
end

module Make (D : Data) = struct

  module D = D

  type t = unit

  let create in_size out_size = failwith "Unimplemented"

  let input_size network = failwith "Unimplemented"

  let layer_size network = failwith "Unimplemented"

  let get_weights network = failwith "Unimplemented"

  let set_weights network weights = failwith "Unimplemented"

  let get_biases network = failwith "Unimplemented"

  let set_biases network biases = failwith "Unimplemented"

  let get_activations network = failwith "Unimplemented"

  let set_activations network acts = failwith "Unimplemented"

  let run network inputs = failwith "Unimplemented"

  let to_string network = failwith "Unimplemented"

  let from_string data = failwith "Unimplemented"

  let copy network = failwith "Unimplemented"

end
