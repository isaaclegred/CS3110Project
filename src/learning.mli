open IO
module type Learning = sig
  (* This could maybe be stored in the parameters file *)
  (* If we have some parameters $ p_i $ and the model is attempting to fit to 
  some data $ f(t_k) $ with some model $ m(t_k, p) $ then the Jacobian is 
  $ \frac{\partial m(t_k, p)} {p_i}  $ I think this is useful in general *)
  type learning_metadata = {minimum : double; Jacobian : IO.mat}
  val learn :  IO.data_file ->  IO.param_file -> (IO.param_file * learning_metadata option)
                                                   
end

