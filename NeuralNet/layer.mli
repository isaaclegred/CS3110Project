
(** A [Layer.t] is a layer in a neural network.  It represents abstractly a map
    from an matrix  [input_size] floats to a matrix of [layer_size floats].  In more depth
    it is a combination of a matrix multiplication, of a matrix (returned by [get_weights]),
    a vector addition with the output of [get_biases], and a list of functions given by 
    [get_activations]. It also serves as an interface for modifying all of these data, and 
    in the process modyfying the neural net the layer is a part of.  
*)
type t


(** [Layer.create weights biases activations activation_derivs] creates a 
   [Layer.t] with [weights], [biases], [activations], and [activation_derivs] as 
   its respective parameters and activations.
*)
val create : Owl.Mat.mat -> Owl.Mat.mat -> (float -> float) list -> (float -> float) list -> t

(** [input_size layer] is the number of inputs into this layer, i.e. this [layer] takes a
    matrix of shape ([input_size layer], 1) as it's input.  
*)
val input_size : t -> int

(** [layer_size layer] is the number of outputs of this layer.*)
val layer_size : t -> int

(** [get_weights layer] returns the weights of  this [layer], it must be 
    a matrix of size ([layer_size layer], [input_size layer)]
*)
val get_weights : t -> Owl.Mat.mat

(** [set_weights new_weights layer] returns an object identical to [layer] 
    but with weights set to [new_weights] 
    raises : [Invalid_argument] if [new_weights] differs in shape from 
    [get_weights layer]
*)
val set_weights : Owl.Mat.mat -> t -> t

(** [incr_weights layer delta_weights] increases the weights of [layer] by 
    [delta_weights] and returns [layer]
    raises : [Invalid_argument] if [new_weights] differs in shape from
    [get_weights layer]
*)
val incr_weights : Owl.Mat.mat -> t -> t

(** [decr_weights layer delta_weights] decreases the weights of [layer] by
    [delta_weights] and returns [layer]
    raises : [Invalid_argument] if [new_weights] differs in shape from
    [get_weights layer]
*)
val decr_weights : Owl.Mat.mat -> t -> t

(** [get_biases layer] returns the biases of this [layer]*)
val get_biases : t -> Owl.Mat.mat

(** [set_biases layer new_biases] changes the biases of [layer] to be the same 
    as [new_biases]
    raises : [Invalid_argument] if [new_biases] differs in shape from
    [get_biases layer]
*)
val set_biases : Owl.Mat.mat -> t -> t

(** [incr_biases layer delta_biases] increments the biases of [layer] by 
    [delta_biases] and returns [layer]
    raises : [Invalid_argument] if [delta_biases] differs in shape from
    [get_biases layer]
*)
val incr_biases : Owl.Mat.mat -> t -> t

(** [decr_biases layer delta_biases] decrements the biases of [layer] by
    [delta_biases] and returns [layer]
    raises : [Invalid_argument] if [delta_biases] differs in shape from
    [get_biases layer]
*)
val decr_biases : Owl.Mat.mat -> t -> t

(** [get_activations layer] returns the activations of [layer] *)
val get_activations : t -> (float -> float) list

(** [set_activations layer new_acts new_act_derivs] returns a new layer 
    identical to [layer] but with activations replaced by [new_acts]
    and activation derivatives replaced by [new_act_derivs] 
    raises: [Invalid_argument] if [new_acts] and [new_act_derivs] differ in shape from 
    [get_activations layer]
*)
val set_activations : (float -> float) list -> (float -> float) list -> t -> t

(** [run input layer] evaluates the function specified by [layer] on the 
    input [input]
    raises: [Invalid_argument] if [input] doesn't have shape [input_size layer]
*)
val run : Owl.Mat.mat -> t -> Owl.Mat.mat

(** [run_with_intermediate] returns a tuple of ([intermediate_result], [result])
    where [result] is [run input layer], and [intermediate_result] is the 
    value returned by adding the biases to [input] and applying the activations
*)
val run_with_intermediate : Owl.Mat.mat -> t -> (Owl.Mat.mat * Owl.Mat.mat)

(** [to_string layer] returns the layer data in the form of a string *)
val to_string : t -> string

(** [from_string layer] takes a string, and returns a layer corresponding to it*)
val from_string : string -> t

(** [copy layer] returns a copy of [layer]*)
val copy : t -> t

(** [print layer] prints the layer*)
val print : t -> unit


 (** [deriv prefactor input target_output partial_eval layer] returns the derivative of 
     the cost function this layer is a member of with respect to its weights, biases, 
     and inputs in the form ((weight_deriv, bias_deriv), input_derivative).  Note 
     [input_deriv] is sometimes called the "lineariazation" of this layer.
*)
val deriv : Owl.Mat.mat -> Owl.Mat.mat -> Owl.Mat.mat -> (Owl.Mat.mat * Owl.Mat.mat) -> t ->
  (Owl.Mat.mat * Owl.Mat.mat) * Owl.Mat.mat                                                                                
