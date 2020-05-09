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
  val default_rate : float
  val create : In.t -> Out.t -> Network.net -> t
  type update_status =
    | Accept of t
    | Reject of t
  val update : t -> update_status
  val train : t -> float -> t
  val get_network : t -> Network.net
end

module MakeDerivative (In : Data) (Out : Data) = struct
  open Canonical_deriv
  module In = In
  module Out = Out
  let eval inputs outputs network =
    let layers = Network.net_layers network in
    let evaled_layers = eval_layers layers inputs in
    let evaled_deriv = eval_derivative layers inputs outputs evaled_layers in
    Array.map fst evaled_deriv
end

module type TrainerMaker =
  functor (In : Data) -> functor (Out : Data) ->
    Trainer with module In = In and module Out = Out
