module Mat = Owl.Mat

let run_test input_size output_size iterations layer_num noise f =

  let module In = struct
    type t = Mat.mat
    let size = input_size (* Number of independent variables *)
    let to_float_array x = Mat.to_array x
  end in

  let module Out = struct
    type t = Mat.mat
    let size = output_size (* Number of dependent variables *)
    let to_float_array x = Mat.to_array x
  end in

  let module T = DefaultTrainer.Make(In)(Out) in

  (** [rnd_seq ()] is an array of length [In.size + Out.size], whose entries
      are [f t] but adjusted by a random factor up to [+/- noise], where [t] is
      the index in the array. *)
  let rnd_seq () =
    let quiet t = t |> Float.of_int |> f in
    let noisy t = quiet t *. (Random.float (noise *. 2.) -. noise +. 1.) in
    Array.init (In.size + Out.size) noisy in

  (** [split n arr] is [(x, y)], where [x] is the first [n] entries of [arr]
      and [y] is the rest of [arr].

      Raises: [Invalid_argument] if [n < 0] or [n > Array.length arr]. *)
  let split arr =
    let n = In.size in
    if n < 0 || n > Array.length arr then
      Invalid_argument "n is too big" |> raise
    else
      let subarray start width a = Array.init width (fun i -> a.(start + i)) in
      subarray 0 n arr, subarray n (Array.length arr - n) arr in

  let input_data, output_data =
    ()
    |> rnd_seq
    |> split
    |> (fun (i, o) -> Mat.of_array i In.size 1, Mat.of_array o Out.size 1) in

  let max_size = In.size + Out.size in

  let layer_sizes = Array.init layer_num (fun i ->
      if i = layer_num - 1 then Out.size else Random.int max_size + 1
    ) in

  let layers = Array.init layer_num (fun i ->
      let input_size = if i = 0 then In.size else layer_sizes.(i - 1) in
      Layer.create
        (Mat.uniform layer_sizes.(i) input_size)
        (Mat.zeros input_size 1)
        (List.init input_size (fun _ x -> x))
        (List.init input_size (fun _ _ -> 1.))
    ) in

  let network =
    Network.create In.size Out.size
    |> (fun x -> Array.fold_left (fun pre_net layer ->
        Network.add_layer layer pre_net) x layers)
    |> Network.seal in

  let trainer = T.create input_data output_data network |> ref in

  let pp_mat name arr =
    print_endline name;
    Mat.iter (fun x -> print_float x; print_string " ") arr |> print_newline;
    arr in

  for i = 1 to iterations do
    print_string "Iteration #"; print_int i; print_endline "\n";
    input_data
    |> pp_mat "Input:"
    |> (!trainer |> T.get_network |> Network.run_mat)
    |> (fun x -> pp_mat "Expected:" output_data |> ignore; pp_mat "Actual:" x)
    |> Trainer.cost_of_mat output_data
    |> (fun x -> print_string "Loss: "; print_float x; print_endline "\n");
    match !trainer |> T.update with
    | Accept better ->
      print_endline "Accepted"; trainer := better
    | Reject worse ->
      print_endline "Rejected"; trainer := worse
  done;
  !trainer
  |> T.get_network
  |> (fun x -> Network.print_net x; x)
