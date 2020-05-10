(** A neural network and associated functions. *)

(** The type of incomplete neural networks. *)
type pre_net

(** The type of complete neural networks. *)
type net

(** [create input_size output_size] is a [pre_net] that takes in input of size
    [input_size] and gives out output of size [output_size]. 

    Requires: *)
val create : int -> int -> pre_net

(** [add_layer layer pre_net] is [pre_net] but with [layer] tacked on.

    Raises: [Invalid_argument] if the shape of [layer] does not match the shape
    of [pre_net]'s tail. *)
val add_layer : Layer.t -> pre_net -> pre_net

val seal : Layer.t -> pre_net -> net

val input_size : net -> int

val output_size : net -> int

(** [run network] runs the neural net [network] on the float array [inputs] to get predicted
    outputs. *)
val run : net -> float array -> float array

val run_mat : net -> Owl.Mat.mat -> Owl.Mat.mat

(** Set the new weights and biases of the net [network] to [new_weights] and
    [new_biases]. *)
val update : Owl.Mat.mat array -> Owl.Mat.mat array -> net -> net

val incr : Owl.Mat.mat array -> Owl.Mat.mat array -> net -> net

val decr : Owl.Mat.mat array -> Owl.Mat.mat array -> net -> net

val to_string : net -> string

val from_string : string -> net

val pre_net_layers : pre_net -> Layer.t array

(* The layers returned are views. Be careful about direct modifications. *)
val net_layers : net -> Layer.t array

val copy_pre_net : pre_net -> pre_net

val copy_net : net -> net

val print_pre_net : pre_net -> unit

val print_net : net -> unit

val to_parameter_list : net -> (Owl.Mat.mat * Owl.Mat.mat) list

val from_parameter_list : (Owl.Mat.mat * Owl.Mat.mat) list -> net
