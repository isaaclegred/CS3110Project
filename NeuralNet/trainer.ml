module Mat = Owl.Mat
let cost expected actual =
  let to_mat arr = Mat.of_array arr (Array.length arr) 1 in
  Mat.((to_mat expected) - (to_mat actual) |> sqr |> sum')
let cost_mat expected actual =
  Mat.(expected-actual |> sqr |> sum')

module type Data = sig
  type t
  val size : int
  val to_float_array : t -> float array
end

module type Derivative = sig
  module In : Data
  module Out : Data
  val eval :
    Mat.mat -> Mat.mat ->
    Network.net ->
    (Mat.mat * Mat.mat) array
end

module type Trainer = sig
  module In : Data
  module Out : Data
  module D : Derivative
  type t
  val learning_rate : float ref
  val create : In.t -> Out.t  -> t
  type update_status =
    | Accept of t
    | Reject of t
  val update : t -> update_status
  val train : t -> t
  val get_network : t -> Network.net
end

module type DerivativeMaker =
  functor (In : Data) -> functor (Out : Data) ->
    Derivative with module In = In and module Out = Out

module type TrainerMaker =
  functor (In : Data) -> functor (Out : Data) ->
    functor (D : Derivative with module In = In and module Out = Out) ->
      Trainer with module In = In and module Out = Out and module D = D
