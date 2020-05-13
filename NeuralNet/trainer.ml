module Mat = Owl.Mat

let cost_of_mat (expected : Mat.mat) (actual : Mat.mat) : float =
  Mat.(expected - actual |> sqr |> sum')

let cost_of_array (expected : float array) (actual : float array) : float =
  let to_mat arr = Mat.of_array arr (Array.length arr) 1 in
  cost_of_mat (to_mat expected) (to_mat actual)

module type Data = sig
  type t
  val size : int
  val to_float_array : t -> float array
end

module type Trainer = sig
  module In : Data
  module Out : Data
  type t
  val default_rate : float
  val create : In.t -> Out.t -> Network.net -> t
  type update_status =
    | Accept of t
    | Reject of t
  val update : t -> update_status
  val train : t -> float -> t
  val get_network : t -> Network.net
  val get_rate : t -> float
end

module type TrainerMaker =
  functor (In : Data) -> functor (Out : Data) ->
    Trainer with module In = In and module Out = Out
