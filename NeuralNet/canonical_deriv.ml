(* Evluate the layers one at a time and store the intermediate results *)
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
(* Now that the function is evaluated at all of the layers computing the derivative is just
   the linear response of the function to the parameters, the derivative at the last parameters 
   is simple, because it is the same as a single net case, but evaluated at the result of the net 
   up to that point.  The second layer is teh same, except then propogated through the 
   deriivatives  to the left of it.  
   BEGIN LATEX: 
   $$f(x) = L_n \circ ... L_2 \circ L_0(x)$$
   Where $x$ are the seen values and $y$ are the unseen
   $$\text{def} \,\,\, f_i(x) := L_i \circ L_{i-1} \circ ... L_0(x) $$ So $f_n(x) =f(x)$, 
   note: this can be evaluated recursively from the ``right"
   $$\frac{\partial f}{\partial L_i} f(x) = L_n'(f_{n-1}(x)) \times 
   L_{n-1}'(f_{n-2}(x)\times ... L_i'(f_{i-1}(x)))  $$
   Note: This can be evaluated recursively from the ``left" 
   because we already know all the $f_i'$
   There Should be a function inside layer which is \texttt{compute\_layer\_derivative layer 
   f\_i} it is exactly the same as the functions for computing bias derivative and weight 
   derivatives as before, and can be extended to have a sigmoid derivative by just composing 
   with the derivative of the sigmoid derivative afterward. 
   END LATEX
*)
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
