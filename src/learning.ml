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

(* Returns an r x s matrix: derivative of cost w.r.t. weights.
   [data] is (seen data (r, 1), unseen data (s, 1)).
   [params] is (weights (r x s), biases (1 x s)). *)
let construct_weight_deriv data params =
  let f = construct_fun_from_params params in
  let residuals = Mat.(f (fst data) - snd data) in
  Mat.(2. $* transpose residuals * fst data)

(* Returns a 1 x s matrix: derivative of cost w.r.t. biases.
   [data] is (seen data (r, 1), unseen data (s, 1)).
   [params] is (weights (r x s), biases (1 x s)). *)
let construct_bias_deriv data params =
  let f = construct_fun_from_params params in
  let residuals = Mat.(f (fst data) - snd data) in
  Mat.(2. $* transpose residuals)

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

module Derivative (In: Trainer.Data) (Out: Trainer.Data) :
  Trainer.Derivative with module In = In and module Out = Out = struct

  module In = In
  module Out = Out

  (* If it's easier to work with the derivative when the array of [Mat]s is
     flattened, then you can use this. *)
  let flatten mat_arr = failwith "Unimplemented"

  (* [inputs] is independent data; [outputs] is dependent data.
     [weights] and [biases] are the weights and biases
     of the layers present, in order.
     [network] is the current network.
     Output should be the derivative at each coordinate, in the same order. *)
  let eval inputs outputs network weights biases = weights, biases (* TODO *)

end

module T = OneLayer.Make(In)(Out)(Derivative(In)(Out))

let run_test count input_size total_size max_multiplier =

  (* Create an array of length [total_size], with entries of the form [at^2],
     where [t] is the index in the array and [a] is a random fixed float. *)
  let rnd_seq _ =
    let a = Random.float max_multiplier in
    Array.init total_size (fun t -> a *. Float.of_int t ** 2.)
  in

  let split n arr =
    if n > Array.length arr then Invalid_argument "n is too big" |> raise else
      let subarray start width a = Array.init width (fun i -> a.(start + i)) in
      (subarray 0 n arr, subarray n (Array.length arr - n) arr)
  in

  (* Create an array of length [count], whose entries are tuples [(x, y)] of
     arrays, [x] of length [input_size] and [y] of length
     [total_size - input_size], such that [append x y] follows an easy trend. *)
  let time_series = Array.init count rnd_seq |> Array.map (split input_size)
  in

  let unzip arr = Array.map fst arr, Array.map snd arr
  in

  let input_data, output_data = unzip time_series
  in

  let one_layer_trainer = T.create input_data output_data |> ref
  in

  for i = 0 to 99 do
    !one_layer_trainer |> T.get_network |> Network.print_net;
    one_layer_trainer := !one_layer_trainer |> T.update
  done;
  !one_layer_trainer |> T.get_network |> Network.print_net

(* [Mat.print] DOES NOT FLUSH PROPERLY SO EVERYTHING'S JUMBLED UP IN UTOP *)

let () = run_test 10 90 100 1000.
