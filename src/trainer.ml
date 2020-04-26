module type Data = sig
  type t
  val size : int
  val to_float_array : t -> float array
end

module type Trainer = sig

  module In : Data
  module Out : Data

  type t

  val create : In.t array -> Out.t array -> Network.net

end

module type TrainerMaker =
  functor (In : Data) -> functor (Out : Data) ->
    Trainer with module In = In and module Out = Out
