type t

val create : Owl.Mat.mat -> Owl.Mat.mat -> (float -> float) list -> t

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
