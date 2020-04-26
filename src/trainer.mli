module type Data = sig
  type t
  val size : int
  val to_float_array : t -> float array
end

module type Derivative = sig
  (* Input is an array of (weights, biases) pairs of the layers present.
     Output should be the derivative in the same order. *)
  val eval :
    (Owl.Mat.mat * Owl.Mat.mat) array -> (Owl.Mat.mat * Owl.Mat.mat) array
end

module type Trainer = sig

  module In : Data
  module Out : Data
  module D : Derivative

  type t

  val learning_rate : float

  val create : In.t array -> Out.t array -> t

  (* Train once *)
  val update : t -> t

  (* Update until some threshold *)
  val train : t -> t

  val get_network : t -> Network.net

end

module type TrainerMaker =
  functor (In : Data) -> functor (Out : Data) -> functor (D : Derivative) ->
    Trainer with module In = In and module Out = Out and module D = D
