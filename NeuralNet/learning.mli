(* This could maybe be stored in the parameters file. *)
(* If we have some parameters $p_i$ and the model is attempting to fit to some
   data $f(t_k)$ with some model $m(t_k, p)$ then the Jacobian is
   $\frac{\partial m(t_k, p)}{p_i}$. I think this is useful in general. *)

module Mat = Owl.Mat
module OneLayerDerivative (In: Trainer.Data) (Out: Trainer.Data) :
  Trainer.Derivative with module In = In and module Out = Out

val run_test :
  int ->
  int ->
  int ->
  float ->
  (float -> float) ->
  Network.net
