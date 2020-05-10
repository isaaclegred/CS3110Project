(** Functionality for training a neural network *)
(** [cost_of_array expected actual] is the cost of [actual] relative to
    [expected]. *)
val cost_of_array : float array -> float array -> float

(** [cost_of_mat expected actual] is the cost of [actual] relative to
    [expected]. *)
val cost_of_mat : Owl.Mat.mat -> Owl.Mat.mat -> float

(** A [Data] is a value that can be used as input or output to a neural net. *)
module type Data = sig

  (** [t] is the type of data. *)
  type t

  (** [size] is the length of [to_float_array t] for all [t]. *)
  val size : int

  (** [to_float_array t] is an array of length [size] representing [t]. *)
  val to_float_array : t -> float array

end

(** A [Trainer] trains neural networks. *)
module type Trainer = sig

  (** [In] is a module representing the type of input data of the net being
      trained and functions on them. *)
  module In : Data

  (** [Out] is a module representing the type of output data of the net being
        trained and functions on them. *)
  module Out : Data

  (** [t] is the type of trainers. *)
  type t

  (** [default_rate] is the default learning rate of trainers. *)
  val default_rate : float

  (** [create in out network] is a trainer for [network] with input data [in]
      and output data [out]. *)
  val create : In.t -> Out.t -> Network.net -> t

  (** [update_status] is the type of update statuses.
      In general, we will only want to actually update the parameters of the
      net if the objective function is smaller after the update. This will
      allow us to use larger step sizes overall and only use a smaller one if
      the larger one fails to produce a useful step. *)
  type update_status =
    | Accept of t
    | Reject of t

  (* [update t] is [Accept t'] if [get_network t'] is at least as good as
     [get_network t] and is [Reject t'] if [get_network t'] is worse than
     [get_network t] (where [t'] is [t] after one training step). *)
  val update : t -> update_status

  (* [train t f] is [t] except that the neural net has been trained until
     its cost on the training data is less than [f]. Note that this function
     might not terminate. *)
  val train : t -> float -> t

  (* [get_network t] is the neural net being trained by [t]. *)
  val get_network : t -> Network.net

end

(** A [TrainerMaker] is a functor that makes a [Trainer] out of modules
    representing input data and output data. *)
module type TrainerMaker =
  functor (In : Data) -> functor (Out : Data) ->
    Trainer with module In = In and module Out = Out
