type pre_net

type net

val create : int -> int -> pre_net

val add_layer : Layer.t -> pre_net -> pre_net

val seal : Layer.t -> pre_net -> net

val input_size : net -> int

val output_size : net -> int

val run : net -> float array -> float array

val update : Owl.Mat.mat array -> Owl.Mat.mat array -> net -> net

val incr : Owl.Mat.mat array -> Owl.Mat.mat array -> net -> net

val to_string : net -> string

val from_string : string -> net

val copy_pre_net : pre_net -> pre_net

val copy_net : net -> net

val print_pre_net : pre_net -> unit

val print_net : net -> unit
