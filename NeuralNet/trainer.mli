module Mat = Owl.Mat
val cost : float array -> float array -> float
val cost_mat : Mat.mat -> Mat.mat -> float
module type Data = sig
  type t
  val size : int
  val to_float_array : t -> float array
end

module type Derivative = sig
  module In : Data
  module Out : Data
  val eval :
    Owl.Mat.mat -> Owl.Mat.mat ->
    Network.net ->
    (Owl.Mat.mat * Owl.Mat.mat) array
end

module type Trainer = sig

  module In : Data
  module Out : Data
  module D : Derivative

  type t
  (* We want to be able to change the learning rate during the training to
     increase convergence speed*)
  val default_rate : float

  val create : In.t -> Out.t -> Network.net -> t

  (* In general we will only want to actually update the parameters if the
     objective function is smaller after the update.  This will allow us to
     use larger step sizes overall and only use a smaller one if the larger
     one fails to produce a useful step. .
  *)
  type update_status =
    | Accept of t
    | Reject of t

  (* Train once *)
  val update : t -> update_status

  (* Update until some threshold *)
  val train : t -> float -> t

  val get_network : t -> Network.net

end

module MakeDerivative :
  functor (In : Data) -> functor (Out : Data) ->
    Derivative with module In = In and module Out = Out

module type TrainerMaker =
  functor (In : Data) -> functor (Out : Data) ->
    Trainer with module In = In and module Out = Out
