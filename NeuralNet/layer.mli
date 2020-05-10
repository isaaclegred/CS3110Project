(** A [t] is a layer in a neural network. Abstractly, it represents a map from
    a vector of [input_size] floats to a vector of [layer_size] floats. In more
    depth, it is a combination of a vector addition with [get_biases], a matrix
    multiplication with [get_weights], and a function application with
    [get_activations]. *)
type t

(** [create weights biases activations activation_derivs] is a [t] with
    [weights], [biases], [activations], and [activation_derivs] as its
    respective parameters and activations.

    Raises: [Invalid_argument] if [weights] has shape [(m, n)] but at least one
    of [biases], [activations], [activation_derivs] does not have length [n]. *)
val create :
  Owl.Mat.mat ->
  Owl.Mat.mat ->
  (float -> float) list ->
  (float -> float) list ->
  t

(** [input_size layer] is the number of inputs into [layer], i.e. [layer] takes
    a matrix of shape [(input_size layer, 1)] as input. *)
val input_size : t -> int

(** [layer_size layer] is the number of neurons in [layer]. *)
val layer_size : t -> int

(** [get_weights layer] is the weights of [layer], a matrix of shape
    [(layer_size layer, input_size layer)]. *)
val get_weights : t -> Owl.Mat.mat

(** [set_weights new_weights layer] is [layer] except with weights set to
    [new_weights].

    Raises: [Invalid_argument] if [new_weights] differs in shape from
    [get_weights layer]. *)
val set_weights : Owl.Mat.mat -> t -> t

(** [incr_weights delta_weights layer] is [layer] but with weights increased by
    [delta_weights].

    Raises: [Invalid_argument] if [delta_weights] differs in shape from
    [get_weights layer]. *)
val incr_weights : Owl.Mat.mat -> t -> t

(** [decr_weights delta_weights layer] is [layer] but with weights decreased by
    [delta_weights].

    Raises: [Invalid_argument] if [delta_weights] differs in shape from
    [get_weights layer]. *)
val decr_weights : Owl.Mat.mat -> t -> t

(** [get_biases layer] is the biases of [layer], a matrix of shape
    [(input_size layer, 1)]. *)
val get_biases : t -> Owl.Mat.mat

(** [set_biases new_biases layer] is [layer] except with biases set to
    [new_biases].

    Raises: [Invalid_argument] if [new_biases] differs in shape from
    [get_biases layer]. *)
val set_biases : Owl.Mat.mat -> t -> t

(** [incr_biases delta_biases layer] is [layer] but with biases increased by
    [delta_biases].

    Raises: [Invalid_argument] if [delta_biases] differs in shape from
    [get_biases layer]. *)
val incr_biases : Owl.Mat.mat -> t -> t

(** [decr_biases delta_biases layer] is [layer] but with biases decreased by
    [delta_biases].

    Raises: [Invalid_argument] if [delta_biases] differs in shape from
    [get_biases layer]. *)
val decr_biases : Owl.Mat.mat -> t -> t

(** [get_biases layer] is the activations of [layer]. *)
val get_activations : t -> (float -> float) list

(** [set_activations new_acts new_act_derivs layer] is [layer] except with
    activations replaced by [new_acts] and activation derivatives replaced by
    [new_act_derivs].

    Requires: [new_act_derivs] is the derivative of [new_acts].

    Raises: [Invalid_argument] if [new_acts] or [new_act_derivs] differ in
    length from [get_activations layer]. *)
val set_activations : (float -> float) list -> (float -> float) list -> t -> t

(** [run input layer] is the result of pushing [input] through [layer].

    Raises: [Invalid_argument] if [input] is not of shape
    [(input_size layer, 1)]. *)
val run : Owl.Mat.mat -> t -> Owl.Mat.mat

(** [run_with_intermediate input layer] is [(intermediate_result, result)],
    where [result] is [run input layer], and [intermediate_result] is the
    value returned by adding the biases to [input] and applying the activations.

    Raises: [Invalid_argument] if [input] is not of shape
    [(input_size layer, 1)]. *)
val run_with_intermediate : Owl.Mat.mat -> t -> Owl.Mat.mat * Owl.Mat.mat

(** [to_string layer] is [layer] as a string. *)
val to_string : t -> string

(** [from_string str] is [layer] if [str] is [to_string layer].

    Raises: [Invalid_argument] if [str] cannot come from [to_string]. *)
val from_string : string -> t

(** [copy layer] is a (deep) copy of [layer]. *)
val copy : t -> t

(** [print layer] prints [layer] to stdout. *)
val print : t -> unit

(** [deriv prefactor desired_output actual_output partial_eval layer] is
    [((weight_deriv, bias_deriv), input_deriv)], the derivative of the cost
    function of the network that [layer] is a member of with respect to its
    weights, biases and inputs, respectively. Note that [input_deriv] is
    sometimes called the "linearization" of this layer. *)
val deriv :
  Owl.Mat.mat ->
  Owl.Mat.mat ->
  Owl.Mat.mat ->
  Owl.Mat.mat * Owl.Mat.mat ->
  t ->
  (Owl.Mat.mat * Owl.Mat.mat) * Owl.Mat.mat
