
module Mat = Owl.Mat
let eval_layers (layers : Layer.t array) (input : Mat.mat)  =
  let num_layers = Array.length layers in
  let evaluated_layers = Array.make (num_layers+1) (Mat.copy input , input) in 
  let rec loop index =
    match index with
    | max when max = num_layers + 1 -> ();
    | j -> evaluated_layers.(j) <- (Layer.run_with_intermediate (snd evaluated_layers.(j-1))
                                      layers.(j-1) ); loop (j + 1);
  in loop 1;
  evaluated_layers

let eval_derivative layers input desired_output evaluated_layers :
  ((Mat.mat * Mat.mat) * Mat.mat) array=
  let num_layers = Array.length layers in
  let out_dim = fst (Mat.shape desired_output) in
  let evaluated_derivs = Array.make num_layers ((Mat.create 1 1 0.0, (Mat.create 1 1 0.0)),
                                                (Mat.create 1 1 0.0) ) in
  let actual_output = snd evaluated_layers.(num_layers) in
  let rec co_loop index =
    match index with
    | final when final = (num_layers - 1)  ->
      evaluated_derivs.(final) <-
        Layer.deriv (Mat.eye out_dim) desired_output actual_output
          (evaluated_layers.(final)) layers.(final);
      if final = 0 then () else co_loop(final);
    | 0  -> evaluated_derivs.(0) <-
        (Layer.deriv (snd evaluated_derivs.(1)) desired_output actual_output
           (input,input) layers.(0) )
    | j -> evaluated_derivs.(j) <- (Layer.deriv (snd evaluated_derivs.(j + 1))
                                      desired_output actual_output (evaluated_layers.(j))
                                      layers.(j)); co_loop (j-1)
  in co_loop (num_layers - 1);
  evaluated_derivs
