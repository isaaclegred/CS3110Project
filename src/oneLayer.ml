open Trainer

module Make (In : Data) (Out : Data) = struct

  module In = In
  module Out = Out

  type t = unit

  let create ins outs =
    Network.create In.size Out.size
    |> ignore

end
