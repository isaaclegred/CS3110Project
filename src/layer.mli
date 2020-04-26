type t

val create : Owl.Mat.mat -> Owl.Mat.mat -> (float -> float) list -> t

val input_size : t -> int

val layer_size : t -> int

val get_weights : t -> Owl.Mat.mat

val set_weights : Owl.Mat.mat -> t -> t

val incr_weights : Owl.Mat.mat -> t -> t

val decr_weights : Owl.Mat.mat -> t -> t

val get_biases : t -> Owl.Mat.mat

val set_biases : Owl.Mat.mat -> t -> t

val incr_biases : Owl.Mat.mat -> t -> t

val decr_biases : Owl.Mat.mat -> t -> t

val get_activations : t -> (float -> float) list

val set_activations : (float -> float) list -> t -> t

val run : Owl.Mat.mat -> t -> Owl.Mat.mat

val to_string : t -> string

val from_string : string -> t

val copy : t -> t

val print : t -> unit
