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

module Make (D : Data) : Network with module D = D
