(* This could maybe be stored in the parameters file. *)
(* If we have some parameters $p_i$ and the model is attempting to fit to some
   data $f(t_k)$ with some model $m(t_k, p)$ then the Jacobian is
   $\frac{\partial m(t_k, p)}{p_i}$. I think this is useful in general. *)

val construct_fun_from_params :
  IO.weights * IO.biases ->
  IO.independent_data ->
  IO.independent_data

val construct_cost :
  IO.independent_data * IO.dependent_data ->
  IO.weights * IO.biases ->
  float

val construct_weight_deriv :
  IO.independent_data * IO.dependent_data ->
  IO.weights * IO.biases ->
  IO.Mat.mat

val construct_bias_deriv :
  IO.independent_data * IO.dependent_data ->
  IO.weights * IO.biases ->
  IO.Mat.mat

module OneLayerDerivative (In: Trainer.Data) (Out: Trainer.Data) :
  Trainer.Derivative with module In = In and module Out = Out

val run_test : int -> int -> int -> int -> float -> unit
