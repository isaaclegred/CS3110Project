(* Evluate the layers one at a time and store the intermediate results *)
module Mat = Owl.Mat
val eval_layers :  Layer.t array -> Mat.mat -> Mat.mat array 

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
val eval_derivative :  Layer.t array -> Mat. mat -> Mat.mat -> Mat.mat array ->
  ((Mat.mat * Mat.mat) * Mat.mat) array 
  
