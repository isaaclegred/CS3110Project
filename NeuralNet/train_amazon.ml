module Mat = Owl.Mat
exception NoData of string

let extract_independent_data path = 
    match IO.(read path R |> unpack_data) with
    | None -> NoData "" |> raise 
    | Some df ->
      begin
        match df.data with
        | None -> NoData "" |> raise
        | Some d -> fst d
      end
      

let amazon_data = extract_independent_data "amazon_price.csv"
(* Create a net with the usual parameters, and aux_layer sizes the layer sizes
   of the layers you want (a list) which are auxiallry in the sense that they are 
   the hidden layers, the final layer is added automatically. 
*)
let make_net input_size output_size aux_layer_sizes=
  
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

  

  let layer_sizes = (match aux_layer_sizes with
    | [] -> [output_size]
    | h::t -> List.rev(output_size::List.rev(h::t))) |> Array.of_list
  in 
  let layer_num  = Array.length layer_sizes in 
  let layers = Array.init layer_num (fun i ->
      let input_size = if i = 0 then In.size else layer_sizes.(i - 1) in
      Layer.create
        (Mat.gaussian ~mu:1.0 ~sigma:0.5 layer_sizes.(i) input_size)
        (Mat.zeros input_size 1)
        (List.init input_size (fun _ x -> x))
        (List.init input_size (fun _ _ -> 1.))
    ) in
    let network =
    Network.create In.size Out.size
    |> (fun x -> Array.fold_left (fun pre_net layer ->
        Network.add_layer layer pre_net) x layers)
    |> Network.seal in
  network

(* The data is just a long time series of daily open prices of amazon  
   we fit it In.size + Out.size at a time, starting on day [start_day]
   i.e. start_day = 20 means that the input and output data is the subset of 
   data starting at element start_day in the array, and going through start_day + 
   In.size + Out.size.
*)
let train_net_on_data network data start_day iterations =
  let input_size = Network.input_size network in
  let output_size = Network.output_size network in
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

  let total_data_points = Out.size + In.size in 
  let relevant_data = Array.init (total_data_points) (fun i -> data.(start_day + i)) in 
  let split arr =
    let n = In.size in
    if n < 0 || n > Array.length arr then
      Invalid_argument "n is too big" |> raise
    else
      let subarray start width a = Array.init width (fun i -> a.(start + i)) in
      subarray 0 n arr, subarray n (Array.length arr - n) arr in

  let input_data, output_data =
    relevant_data  
    |> split
    |> (fun (i, o) -> Mat.of_array i In.size 1, Mat.of_array o Out.size 1) in
  
  let module T = DefaultTrainer.Make(In)(Out) in

  let pp_mat name arr =
    print_endline name;
    Mat.iter (fun x -> print_float x; print_string " ") arr |> print_newline;
    arr in
  let trainer = T.create input_data output_data network |> ref in
  let counter = ref 0 in
  while !counter < iterations do
    
    if !counter mod 10 = 0 then (print_string "learning rate is";
    print_float (T.get_rate !trainer));
    print_string "Iteration #"; print_int !counter; print_endline "\n";
    input_data
    (*|> pp_mat "Input:"*)
    |> (!trainer |> T.get_network |> Network.run_mat)
    (*|> (fun x -> pp_mat "Expected:" output_data |> ignore; pp_mat "Actual:" x)*)
    |> Trainer.cost_of_mat output_data
    |> (fun x -> print_string "Loss: "; print_float x; print_endline "\n");
    match !trainer |> T.update with
    | Accept better ->
      print_endline "Accepted"; counter:= !counter + 1; trainer := better
    | Reject worse ->
      print_endline "Rejected"; trainer := worse
  done;
  !trainer
  |> T.get_network
  |> (fun x ->  x)

let train_net_to_tol network data start_day tol =
  let input_size = Network.input_size network in
  let output_size = Network.output_size network in
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

  let total_data_points = Out.size + In.size in 
  let relevant_data = Array.init (total_data_points) (fun i -> data.(start_day + i)) in 
  let split arr =
    let n = In.size in
    if n < 0 || n > Array.length arr then
      Invalid_argument "n is too big" |> raise
    else
      let subarray start width a = Array.init width (fun i -> a.(start + i)) in
      subarray 0 n arr, subarray n (Array.length arr - n) arr in

  let input_data, output_data =
    relevant_data  
    |> split
    |> (fun (i, o) -> Mat.of_array i In.size 1, Mat.of_array o Out.size 1) in
  
  let module T = DefaultTrainer.Make(In)(Out) in
  let trainer = T.create input_data output_data network |> ref in
  trainer := (T.train !trainer tol); 
  !trainer
  |> T.get_network
       
let run_program mat_data =
  let data = Mat.to_array mat_data in
  let ins = 10 in
  let outs = 2 in
  let net = make_net ins outs [4;5;2] in
  let net_1 = train_net_to_tol net (data) 0 10. in 
  let net_11 = train_net_to_tol net_1 (data) 30 8. in
  let net_2 = train_net_to_tol net_11 (data) 60 8. in
  let net_3 = train_net_to_tol net_2 (data) 170 20. in
  let net_4 = train_net_to_tol net_3 (data) 205 20. in
  let net_41 = train_net_to_tol net_4 (data) 190 20. in
  let net_5 = train_net_to_tol net_41 (data) 210 20. in
  let net_6 = train_net_to_tol net_5 (data) 210 8. in
  let net_7 = train_net_to_tol net_6 (data) 120 8. in
  let net_8 = train_net_to_tol net_7 (data) 205 8. in
  let net_9 = train_net_to_tol net_8 (data) 190 8. in
  let net_10 = train_net_to_tol net_9 (data) 240 2. in
  Network.run net_10 (Array.sub data (252-ins) ins)
