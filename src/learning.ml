module M = Owl.Mat

let construct_fun_from_params params input =
  M.(fst params *@ input + snd params)

let construct_cost data params =
  let f = construct_fun_from_params params in
  let residuals = M.(f (fst data) - snd data) in
  M.(residuals |> sqr |> sum')

(* Returns an r x s matrix: derivative of cost w.r.t. weights.
   [data] is (seen data (r, 1), unseen data (s, 1)).
   [params] is (weights (r x s), biases (1 x s)). *)
let construct_weight_deriv data params =
  let f = construct_fun_from_params params in
  let residuals = M.(f (fst data) - snd data) in
  M.(2. $* fst data *@ transpose residuals)

(* Returns a 1 x s matrix: derivative of cost w.r.t. biases.
   [data] is (seen data (r, 1), unseen data (s, 1)).
   [params] is (weights (r x s), biases (1 x s)). *)
let construct_bias_deriv data params =
  let f = construct_fun_from_params params in
  let residuals = M.(f (fst data) - snd data) in
  M.(2. $* transpose residuals)

(* Example of how to use the Trainer module, specifically the OneLayer
   implementation. The network in OneLayer is a single layer network with one
   neuron per output (dependent) variable. The weights are initialized
   uniformly at random, and the biases are initialized to zero. *)

module OneLayerDerivative (In: Trainer.Data) (Out: Trainer.Data) :
  Trainer.Derivative with module In = In and module Out = Out = struct

  module In = In
  module Out = Out

  let convert inputs outputs weight bias =
    let number_of_points = M.col_num inputs in
    let valid =
      number_of_points = M.col_num outputs
      && M.shape weight = (M.row_num bias, M.row_num inputs) in
    if not valid then Invalid_argument "Invalid sizes/lengths" |> raise
    else
      let make f = Array.init number_of_points f in
      let data = make (fun i -> M.col inputs i, M.col outputs i) in
      make (fun i -> data.(i), (M.copy weight, M.copy bias))

  (* [inputs] is independent data; [outputs] is dependent data.
     [weights] and [biases] are the weights and biases
     of the layers present, in order.
     [network] is the current network.
     Output should be the derivative at each coordinate, in the same order.

     Requires: [weights] and [biases] have length [1]. *)
  let eval inputs outputs network weights biases =
    let data = convert inputs outputs weights.(0) biases.(0) in
    let apply f = Array.map (fun (x, y) -> f x y |> M.transpose) data in
    let wd = apply construct_weight_deriv in
    let bd = apply construct_bias_deriv in
    let mean mats =
      let n = mats |> Array.length |> Float.of_int in
      let r, c = M.shape mats.(0) in
      let ans = [| M.zeros r c |] in
      Array.iter (fun mat -> ans.(0) <- M.(ans.(0) + mat /$ n)) mats;
      ans in
    mean wd, mean bd

end

let run_test count input_size output_size iterations max_multiplier =

  let module In = struct
    type t = float array
    let size = input_size (* Number of independent variables *)
    let to_float_array x = x
  end
  in

  let module Out = struct
    type t = float array
    let size = output_size (* Number of dependent variables *)
    let to_float_array x = x
  end
  in

  let module T = OneLayer.Make(In)(Out)(OneLayerDerivative(In)(Out))
  in

  (* Create an array of length [In.size + Out.size], whose entries are [at^2],
     where [t] is the index in the array and [a] is a random fixed float. *)
  let rnd_seq _ =
    let a = Random.float max_multiplier in
    Array.init (In.size + Out.size) (fun t -> a *. Float.of_int t ** 2.)
  in

  let split n arr =
    if n > Array.length arr then Invalid_argument "n is too big" |> raise else
      let subarray start width a = Array.init width (fun i -> a.(start + i)) in
      (subarray 0 n arr, subarray n (Array.length arr - n) arr)
  in

  (* Create an array of length [count], whose entries are tuples [(x, y)] of
     arrays, [x] of length [In.size] and [y] of length [Out.size], such that
     [append x y] follows an easy trend. *)
  let time_series = Array.init count rnd_seq |> Array.map (split In.size)
  in

  let unzip arr = Array.map fst arr, Array.map snd arr
  in

  let input_data, output_data = unzip time_series
  in

  let one_layer_trainer = T.create input_data output_data |> ref
  in

  let print_arr arr =
    Array.iter (fun x -> print_float x; print_string " ") arr |> print_newline;
    arr
  in

  let pp_arr2 name arr =
    print_endline name;
    Array.iter (fun x -> print_arr x |> ignore) arr |> print_newline;
    arr
  in

  for i = 1 to iterations do
    print_string "Iteration #"; print_int i; print_endline "\n";
    input_data
    |> pp_arr2 "Input:"
    |> Array.map (!one_layer_trainer |> T.get_network |> Network.run)
    |> (fun x -> pp_arr2 "Expected:" output_data |> ignore; pp_arr2 "Actual:" x)
    |> Array.map2 Trainer.cost output_data
    |> Array.fold_left (-.) 0.
    |> (fun x -> print_string "Loss: "; print_float x; print_endline "\n");
    (* !one_layer_trainer |> T.get_network |> Network.print_net; *)
    one_layer_trainer := !one_layer_trainer |> T.update
  done;
  !one_layer_trainer |> T.get_network |> Network.print_net

(* [Mat.print] DOES NOT FLUSH PROPERLY SO EVERYTHING'S JUMBLED UP IN UTOP *)

(* let () = run_test 10 9 1 1000 1. *)
