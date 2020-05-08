module Mat = Owl.Mat
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
  Mat.(2. $* fst data *@ transpose residuals)

(* Returns a 1 x s matrix: derivative of cost w.r.t. biases.
   [data] is (seen data (r, 1), unseen data (s, 1)).
   [params] is (weights (r x s), biases (1 x s)). *)
let construct_bias_deriv data params =
  let f = construct_fun_from_params params in
  let residuals = Mat.(f (fst data) - snd data) in
  Mat.(2. $* transpose residuals)


(* Example of how to use the Trainer module, specifically the OneLayer
   implementation. The network in OneLayer is a single layer network with one
   neuron per output (dependent) variable. The weights are initialized
   uniformly at random, and the biases are initialized to zero. *)

module OneLayerDerivative (In: Trainer.Data) (Out: Trainer.Data) :
  Trainer.Derivative with module In = In and module Out = Out = struct

  module In = In
  module Out = Out

  
  (* [inputs] is independent data; [outputs] is dependent data.
     [weights] and [biases] are the weights and biases
     of the layers present, in order.
     [network] is the current network.
     Output should be the derivative at each coordinate, in the same order.

     Requires: [weights] and [biases] have length [1]. *)
  let eval inputs outputs network =
    let layers  = Network.net_layers network in
    print_endline "about to evaluate layers in learning.ml";
    let evaluated_layers  = Canonical_deriv.eval_layers
        (layers) (inputs) in
    print_endline "evaluated layers";
    let evaluated_derivatives = Canonical_deriv.eval_derivative
        layers inputs outputs evaluated_layers in
    print_endline "evaluated derivatives";
    Array.map (fun pair -> fst pair) evaluated_derivatives
end
 
let run_test input_size output_size iterations noise f =
  let module In = struct
    type t = Mat.mat
    let size = input_size (* Number of independent variables *)
    let to_float_array x = Mat.to_array x
  end
  in

  let module Out = struct
    type t = Mat.mat
    let size = output_size (* Number of dependent variables *)
    let to_float_array x = Mat.to_array x
  end
  in

  let module OneLayerT = OneLayer.Make(In)(Out)(OneLayerDerivative(In)(Out))
  in
  
  (* Create an array of length [In.size + Out.size], whose entries are [f t]
     but adjusted by a random factor up to [+/- noise], where [t] is the
     index in the array. *)
  let rnd_seq _ =
    let quiet t = t |> Float.of_int |> f in
    let noisy t = quiet t *. (Random.float (noise *. 2.) -. noise +. 1.) in
    Array.init (In.size + Out.size) noisy
  in

  let split n arr =
    if n > Array.length arr then Invalid_argument "n is too big" |> raise else
      let subarray start width a = Array.init width (fun i -> a.(start + i)) in
      (subarray 0 n arr, subarray n (Array.length arr - n) arr)
  in
  (* Create an array of length [count], whose entries are tuples [(x, y)] of
     arrays, [x] of length [In.size] and [y] of length [Out.size], such that
     [append x y] follows an easy trend. *)
  let time_series =  rnd_seq 0 |> split In.size 
  in

  let unzip arr = fst arr,  snd arr
  in
 
  let input_data_a, output_data_a = unzip time_series
  in
  let input_data = Mat.of_array input_data_a input_size 1 in
  let output_data = Mat.of_array output_data_a output_size 1 in
 
  let one_layer_trainer = OneLayerT.create input_data output_data |> ref
  in
  (* let print_arr arr =
   *   Mat.iter (fun x -> print_float x; print_string " ") arr |> print_newline;
   *   arr
   * in *)
  let pp_mat name arr =
    print_endline name;
    Mat.iter (fun x -> print_float x; print_string " ") arr |> print_newline; 
    arr
  in 
  (* let pp_arr2 name arr =
   *   print_endline name;
   *   Array.iter (fun x -> print_arr x |> ignore) arr |> print_newline;
   *   arr
   * in *)
  for i = 1 to iterations do
    print_string "Iteration #"; print_int i; print_endline "\n";
    input_data
    |> pp_mat "Input:"
    |> (!one_layer_trainer |> OneLayerT.get_network |> Network.run_mat)
    |> (fun x -> pp_mat "Expected:" output_data |> ignore; pp_mat "Actual:" x)
    |> Trainer.cost_mat output_data
    |> (fun x -> print_string "Loss: "; print_float x ; print_endline "\n");
    (* !one_layer_trainer |> T.get_network |> Network.print_net; *)
    match !one_layer_trainer |> OneLayerT.update with
    | OneLayerT.Reject worse ->  print_endline "Rejected";
                                  ignore OneLayerT.(learning_rate := !learning_rate /. 2.0);
    | OneLayerT.Accept better -> print_endline "accepted";
      ignore OneLayerT.(learning_rate := !learning_rate *. 1.05);
      (one_layer_trainer := better);
  done;
  !one_layer_trainer |> OneLayerT.get_network |> (fun x -> Network.print_net x; x)

(* [Mat.print] DOES NOT FLUSH PROPERLY SO EVERYTHING'S JUMBLED UP IN UTOP *)

 let _ = run_test 9 1 1000 0.05 (fun x -> x ** 2.) 

(* run_test count input_size output_size iterations noise f *)