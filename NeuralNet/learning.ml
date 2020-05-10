module Mat = Owl.Mat

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

  let module T = DefaultTrainer.Make(In)(Out)
  in

  (** [rnd_seq ()] is an array of length [In.size + Out.size], whose entries
      are [f t] but adjusted by a random factor up to [+/- noise], where [t] is
      the index in the array. *)
  let rnd_seq () =
    let quiet t = t |> Float.of_int |> f in
    let noisy t = quiet t *. (Random.float (noise *. 2.) -. noise +. 1.) in
    Array.init (In.size + Out.size) noisy
  in

  (** [split n arr] is [(x, y)], where [x] is the first [n] entries of [arr]
      and [y] is the rest of [arr].

      Raises: [Invalid_argument] if [n < 0] or [n > Array.length arr]. *)
  let split arr =
    let n = In.size in
    if n < 0 || n > Array.length arr then
      Invalid_argument "n is too big" |> raise
    else
      let subarray start width a = Array.init width (fun i -> a.(start + i)) in
      subarray 0 n arr, subarray n (Array.length arr - n) arr
  in

  let input_data, output_data =
    ()
    |> rnd_seq
    |> split
    |> (fun (i, o) -> Mat.of_array i In.size 1, Mat.of_array o Out.size 1)
  in

  let layer_size = 12
  in

  let input_layer =
    Layer.create
      (Mat.uniform layer_size In.size)
      (Mat.zeros In.size 1)
      (List.init In.size (fun _ -> fun x -> x))
      (List.init In.size (fun _ -> fun x -> 1.0))
  in

  let output_layer =
    Layer.create
      (Mat.uniform Out.size layer_size)
      (Mat.zeros layer_size 1)
      (List.init layer_size (fun _ -> fun x -> x))
      (List.init layer_size (fun _ -> fun x -> 1.0))
  in

  let network = Network.(
      create In.size Out.size
      |> add_layer input_layer
      |> seal output_layer
    )
  in

  let trainer = T.create input_data output_data network |> ref
  in

  let pp_mat name arr =
    print_endline name;
    Mat.iter (fun x -> print_float x; print_string " ") arr |> print_newline;
    arr
  in

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
