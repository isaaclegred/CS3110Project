let cost expected actual =
  let to_mat arr = Owl.Mat.of_array arr (Array.length arr) 1 in
  Owl.Mat.((to_mat expected) - (to_mat actual) |> sqr |> sum')

module type Data = sig
  type t
  val size : int
  val to_float_array : t -> float array
end

module type Derivative = sig
  val eval :
    Owl.Mat.mat array -> Owl.Mat.mat array ->
    Owl.Mat.mat array * Owl.Mat.mat array
end

module type Trainer = sig
  module In : Data
  module Out : Data
  module D : Derivative
  type t
  val learning_rate : float
  val create : In.t array -> Out.t array -> t
  val update : t -> t
  val train : t -> t
  val get_network : t -> Network.net
end

module type TrainerMaker =
  functor (In : Data) -> functor (Out : Data) -> functor (D : Derivative) ->
    Trainer with module In = In and module Out = Out and module D = D
