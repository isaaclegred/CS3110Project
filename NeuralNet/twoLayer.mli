(* This functor is a valid TrainerMaker for a single layer net, that is, it can be used
   to update an net if given: [In],  input data, [Out] output data, and a [Derivative] a 
   Derivative module, all defined in trainer.ml 
*)
module Make : Trainer.TrainerMaker

val construct_fun_from_params :
  (* In principle this should be a network?  *)
  IO.weights * IO.biases ->
  IO.independent_data ->
  IO.independent_data

val construct_cost :
  IO.independent_data * IO.dependent_data ->
  (* In principle this should be a network?  *)
  IO.weights * IO.biases ->
  float

val construct_weight_deriv :
  IO.independent_data * IO.dependent_data ->
  (* In principle this should be a network?  *)
  IO.weights * IO.biases ->
  IO.Mat.mat

val construct_bias_deriv :
  IO.independent_data * IO.dependent_data ->
  IO.weights * IO.biases ->
  IO.Mat.mat

