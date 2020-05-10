(** [eval_layers layers input] returns an array [evaluations] that satisfies
    [evaluations.(i)] = [Layer.run_with_intermediate] [layers.(i)] [evaluations.(i-1)]
    and [evaluation.(0)] = (input, input)
*)
module Mat = Owl.Mat
val eval_layers :  Layer.t array -> Mat.mat -> (Mat.mat * Mat.mat) array 

(** Brief : [eval_derivative layers input target_output evaluated_layers] is an
    array [derivs] of [((weight_deriv, bias_deriv), linearization)]s
    Details : The basic principle of the neural net is that each layer is its own 
    function from inputs -> outputs and when streamed together, become one large 
    function [input -> input_1 -> input_2 -> input_3 -> predicted_output] where 
    each [->] represents a layer.  In order to adjust the model via training, it
    is necessary to know how [predicted_output] responds to changes in each of 
    the layers.  [weight_deriv] is the derivative of the cost function
    with respect to the weights of the layer, [bias_deriv] derivative of the
    cost function with respect to the biases of this layer, and [linearization]
    is the derivative of [input_i] with respect to [input_{i-1}] for layer i. 

    
   
*)
val eval_derivative :  Layer.t array -> Mat.mat  ->Mat. mat -> (Mat.mat * Mat.mat) array
  -> ((Mat.mat * Mat.mat) * Mat.mat) array 
