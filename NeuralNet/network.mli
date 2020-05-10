(** A neural network and associated functions. *)

(** The type of incomplete neural networks. *)
type pre_net

(** The type of complete neural networks. *)
type net

(** [create input_size output_size] is a [pre_net] that takes in input of size
    [input_size] and gives out output of size [output_size].

    Raises: [Invalid_argument] if either size is negative. *)
val create : int -> int -> pre_net

(** [add_layer layer pre_net] is [pre_net] but with [layer] tacked on.

    Raises: [Invalid_argument] if the shape of [layer] does not match the shape
    of [pre_net]'s tail. *)
val add_layer : Layer.t -> pre_net -> pre_net

(** [seal pre_net] is [pre_net] but as a [net].

    Raises: [Invalid_argument] if the shape of [pre_net]'s tail does not match
    its [output_size]. *)
val seal : pre_net -> net

(** [input_size network] is the size of [network]'s input. *)
val input_size : net -> int

(** [output_size network] is the size of [network]'s output. *)
val output_size : net -> int

(** [run network inputs] is the result of pushing [inputs] through [network].

    Raises: [Invalid_argument] if the length of [inputs] is not
    [input_size network]. *)
val run : net -> float array -> float array

(** [run_mat network inputs] is the result of pushing [inputs] through
    [network].

    Raises: [Invalid_argument] if [inputs] is not a column vector of size
    [(input_size network, 1)]. *)
val run_mat : net -> Owl.Mat.mat -> Owl.Mat.mat

(** [update weights biases network] is [network] but with weights and biases
    set to [weights] and [biases], respectively.

    Raise: [Invalid_argument] if the shapes don't match. *)
val update : Owl.Mat.mat array -> Owl.Mat.mat array -> net -> net

(** [incr weights biases network] is [network] but with weights and biases
    increased by [weights] and [biases], respectively.

    Raise: [Invalid_argument] if the shapes don't match. *)
val incr : Owl.Mat.mat array -> Owl.Mat.mat array -> net -> net

(** [decr weights biases network] is [network] but with weights and biases
    decreased by [weights] and [biases], respectively.

    Raise: [Invalid_argument] if the shapes don't match. *)
val decr : Owl.Mat.mat array -> Owl.Mat.mat array -> net -> net

(* [pre_net_layers pre_net] are the layers of [pre_net].

   The layers returned are views. Be careful about direct modifications. *)
val pre_net_layers : pre_net -> Layer.t array

(* [net_layers network] are the layers of [network].

   The layers returned are views. Be careful about direct modifications. *)
val net_layers : net -> Layer.t array

(** [copy_pre_net pre_net] is a (deep) copy of [pre_net]. *)
val copy_pre_net : pre_net -> pre_net

(** [copy_net network] is a (deep) copy of [network]. *)
val copy_net : net -> net

(** [print_pre_net pre_net] prints [pre_net] to stdout. *)
val print_pre_net : pre_net -> unit

(** [print_net network] prints [network] to stdout. *)
val print_net : net -> unit

(** [to_parameter_list network] are the weights and biases of [network]. *)
val to_parameter_list : net -> (Owl.Mat.mat * Owl.Mat.mat) list

(** [from_parameter_list params] is [network] if [params] is
    [to_parameter_list network].

    Requires: [params] came from [to_parameter_list]. *)
val from_parameter_list : (Owl.Mat.mat * Owl.Mat.mat) list -> net
