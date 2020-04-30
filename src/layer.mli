
(** A [Layer.t] is a layer in a neural network.  It represents abstractly a map
    from an matrix  [input_size] floats to a matrix of [layer_size floats].  In more depth
    it is a combination of a matrix multiplication, of a matrix (returned by [get_weights]),
    a vector addition with the output of [get_biases], and a list of functions given by 
    [get_activations]. It also serves as an interface for modifying all of these data, and 
    in the process modyfying the neural net the layer is a part of.  
*)
type t

(* Can we change the types in here to be [IO.Mat.mat]s or something to hide the implementation? *)

(* What are activations? *)

(* Create a layer with [weights], [biases], [activations] *)
val create : Owl.Mat.mat -> Owl.Mat.mat -> (float -> float) list -> t

val input_size : t -> int

(* The number of outputs of the layer, also the size of get [get_biases t] *)
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

(* Return the predicted [output] of this layer based on the [input] *)
val run : Owl.Mat.mat -> t -> Owl.Mat.mat

val to_string : t -> string

val from_string : string -> t

val copy : t -> t

val print : t -> unit
