module Mat = Owl.Mat

(* Example of how to use the Trainer module, specifically the OneLayer
   implementation. The network in OneLayer is a single layer network with one
   neuron per output (dependent) variable. The weights are initialized
   uniformly at random, and the biases are initialized to zero. *)

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

  let module OneLayerT = DefaultTrainer.Make(In)(Out)
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

  let layer =
    Layer.create
      (Mat.uniform Out.size In.size)
      (Mat.zeros In.size 1)
      (List.init In.size (fun _ -> (* TODO Owl.Maths.sigmoid *) fun x -> x))
      (List.init In.size (fun _ -> (* TODO Owl.Maths.sigmoid' *) fun x -> 1.0))
  in

  let network = Network.(
      create In.size Out.size
      |> seal layer
    )
  in

  let one_layer_trainer = OneLayerT.create input_data output_data network |> ref
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
    |> Trainer.cost_of_mat output_data
    |> (fun x -> print_string "Loss: "; print_float x ; print_endline "\n");
    (* !one_layer_trainer |> T.get_network |> Network.print_net; *)
    match !one_layer_trainer |> OneLayerT.update with
    | OneLayerT.Reject worse ->
      print_endline "Rejected"; (one_layer_trainer := worse)
    | OneLayerT.Accept better ->
      print_endline "Accepted"; (one_layer_trainer := better)
  done;
  !one_layer_trainer |> OneLayerT.get_network |> (fun x -> Network.print_net x; x)

(* [Mat.print] DOES NOT FLUSH PROPERLY SO EVERYTHING'S JUMBLED UP IN UTOP *)

let _ = run_test 9 1 1000 0.05 (fun x -> x ** 2.)

(* run_test count input_size output_size iterations noise f *)
