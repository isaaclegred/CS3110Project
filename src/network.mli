type pre_net

type net

val create : int -> int -> pre_net

val add_layer : pre_net -> Layer.t -> pre_net

val seal : pre_net -> Layer.t -> net

val input_size : net -> int

val output_size : net -> int

val run : net -> float array -> float array

val to_string : net -> string

val from_string : string -> net

val copy_pre_net : pre_net -> pre_net

val copy_net : net -> net

val print_pre_net : pre_net -> unit

val print_net : net -> unit
