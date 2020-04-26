open IO

module Mat = Owl.Mat

type learning_metadata = {
  minimum : float;
  jacobian : Mat.mat;
}

let construct_fun_from_params params input =
  Mat.(fst params *@ input + snd params)

let construct_cost data params =
  let f = construct_fun_from_params params in
  let residuals = Mat.(f (fst data) - snd data) in
  Mat.(residuals |> sqr |> sum')

let construct_deriv data params = Mat.(fst data @= Mat.ones 1 1)

let learn data params = (params, Some {minimum = 0.; jacobian = Mat.zeros 2 2})

(* NEW STUFF: Example of how to use the Trainer module, specifically the
   OneLayer implementation. The network in OneLayer is a single layer network
   with one neuron per output (dependent) variable. The weights are initialized
   uniformly at random, and the biases are initialized to zero.

   TODO: Please help fill in the derivative, I need to go to sleep. There may
   be bugs, if so I will try to fix them when I wake up. *)

module In = struct
  type t = float array
  let size = 1 (* Number of independent variables *)
  let to_float_array x = x
end

module Out = struct
  type t = float array
  let size = 1 (* Number of dependent variables *)
  let to_float_array x = x
end

module Derivative : Trainer.Derivative = struct

  (* If it's easier to work with the derivative when the array of [Mat]s is
     flattened, then you can use this. *)
  let flatten mat_arr = failwith "Unimplemented"

  (* [inputs] is independent data; [outputs] is dependent data.
     [weights] and [biases] are the weights and biases
     of the layers present, in order.
     Output should be the derivative at each coordinate, in the same order.
     There is a $\chi^2$ cost function at [Trainer.cost], if needed. *)
  let eval inputs outputs weights biases = (weights, biases) (* TODO *)

end

module T = OneLayer.Make(In)(Out)(Derivative)

let input_data = [| [|0.0|]; [|1.0|]; [|2.0|] |]

let output_data = [| [|0.0|]; [|1.0|]; [|2.0|] |]

let one_layer_trainer = T.create input_data output_data |> ref

let () =
  for i = 0 to 99 do
    !one_layer_trainer |> T.get_network |> Network.print_net;
    one_layer_trainer := !one_layer_trainer |> T.update
  done;
  !one_layer_trainer |> T.get_network |> Network.print_net

(* [Mat.print] DOES NOT FLUSH PROPERLY SO EVERYTHING'S JUMBLED UP IN UTOP *)
