module M = Owl.Mat
open Trainer

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

module Make (In : Data) (Out : Data)
    (D : Derivative with module In = In and module Out = Out) = struct

  module M = Owl.Mat
  module In = In
  module Out = Out
  module D = D

  type t = {
    input : M.mat;
    output : M.mat;
    network : Network.net;
    deriv :
      Network.net ->
      M.mat array -> M.mat array ->
      M.mat array * M.mat array
  }

  let learning_rate = ref 0.00001 (* Arbitrarily chosen *)

  let layer =
    Layer.create
      (M.uniform Out.size In.size)
      (M.zeros Out.size 1)
      (List.init Out.size (fun _ -> (* TODO Owl.Maths.sigmoid *) fun x -> x))

  let create ins outs =
    if Array.length ins <> Array.length outs then
      Invalid_argument "Inputs and outputs must be the same length" |> raise
    else
      let to_mat arrs = arrs |> M.of_arrays |> M.transpose in
      let input = ins |> Array.map In.to_float_array |> to_mat in
      let output = outs |> Array.map Out.to_float_array |> to_mat in
      let network =
        Network.(
          create In.size Out.size
          |> seal layer
        ) in
      {input; output; network; deriv = D.eval input output}

  type update_status =
    | Accept of t
    | Reject of t
  let compute_cost {input; output; network; deriv} =
    let layer = (Network.net_layers network).(0)  in
    let params = Layer.(get_weights layer, get_biases layer) in
    construct_cost (input, output) params
  let update ({input; output; network; deriv} as training) =

    (* (input |> Array.map Network.run network) |>
     * Array.map2 cost output |> Array.fold_left (+.) 0. *)
    let original_cost  =  compute_cost training in 
    let f = !learning_rate |> M.($*) |> Array.map in
    let layers = network |> Network.net_layers in
    let ws = layers |> Array.map Layer.get_weights in
    let bs = layers |> Array.map Layer.get_biases in
    let weights, biases = deriv network ws bs |> fun (x, y) -> f x, f y in
    let new_network = Network.decr weights biases network in
    let attempt = {input; output;
                   network = new_network; deriv} in
    let final_cost = compute_cost attempt in
    (* print_endline ("Initial Cost " ^ string_of_float original_cost); *)
    (*  print_endline ("Final Cost " ^ string_of_float final_cost); *)
  if (final_cost > original_cost) then Reject {input; output; network; deriv}
    else Accept attempt

  let train {input; output; network; deriv} = failwith "Unimplemented"

  let get_network {input; output; network; deriv} = network

end
