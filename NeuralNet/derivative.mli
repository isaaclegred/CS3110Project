(** Computes the derivative of the cost function of a network. *)

(** [eval input output network] is an array [derivs] of length
    [network |> Network.net_layers |> Array.length], whose [i]th entry is
    [(weight_deriv, bias_deriv)], where
    [weight_deriv] is the derivative of the weights at layer [i], and
    [bias_deriv] is the derivative of the biases at layer [i].

    In greater detail:

    The basic principle of the neural net is that each layer is its own
    function from inputs to outputs and when streamed together, become one
    large function [input -> input_1 -> ... -> input_n -> predicted_output],
    where each [->] represents a layer.

    In order to adjust the model via training, it is necessary to know how
    [predicted_output] responds to changes in each of the layers.
    [weight_deriv] is the derivative of the cost function with respect to the
    weights of the layer, and
    [bias_deriv] is the derivative of the cost function with respect to the
    biases of the layer.

    Requires:
    - [Network.input_size network = Owl.Mat.row_num input], and
    - [Network.output_size network = Owl.Mat.row_num output]. *)
val eval :
  Owl.Mat.mat ->
  Owl.Mat.mat ->
  Network.net ->
  (Owl.Mat.mat * Owl.Mat.mat) array
