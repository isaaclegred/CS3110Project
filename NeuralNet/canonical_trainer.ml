module M = Owl.Mat
open Trainer

module Make (In : Data) (Out : Data) = struct

  module M = Owl.Mat
  module In = In
  module Out = Out
  module D = Trainer.MakeDerivative(In)(Out)

  type t = {
    input : M.mat;
    output : M.mat;
    network : Network.net;
    deriv : Network.net -> (M.mat * M.mat) array;
    rate : float;
  }

  let default_rate = 0.001 (* Arbitrarily chosen *)

  let create ins outs network =
    let to_mat arr size = arr |> M.of_array size 1 |> M.transpose in
    let input = ins |> In.to_float_array |> to_mat (In.size) in
    let output = outs |> Out.to_float_array |> to_mat (Out.size) in
    {input; output; network; deriv = D.eval input output; rate = default_rate}

  type update_status =
    | Accept of t
    | Reject of t

  let compute_cost {input; output; network; deriv; rate} =
    cost_mat output (Network.run_mat network input)

  let update ({input; output; network; deriv; rate} as training) =
    (* (input |> Array.map Network.run network) |>
     * Array.map2 cost output |> Array.fold_left (+.) 0. *)
    let original_cost = compute_cost training in
    let lr = rate in
    let f = (fun (w, b) -> M.(( lr $* w, lr $* b ))) |> Array.map in
    let dparams = deriv network |> f in
    let new_network = Network.decr (Array.map fst dparams)
        (Array.map snd dparams) network in
    let attempt = {input; output;
                   network = new_network; deriv; rate} in
    let final_cost = compute_cost attempt in
    (* print_endline ("Initial Cost " ^ string_of_float original_cost); *)
    (*  print_endline ("Final Cost " ^ string_of_float final_cost); *)
    if (final_cost > original_cost) then
      Reject {input; output; network; deriv; rate = rate /. 2.}
    else
      Accept {input; output; network = new_network; deriv; rate = rate *. 1.05}

  let rec train trainer threshold =
    if compute_cost trainer < threshold then
      trainer
    else
      match update trainer with
      | Accept new_trainer | Reject new_trainer -> train new_trainer threshold

  let get_network {input; output; network; deriv; rate} = network

end
