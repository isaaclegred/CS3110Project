open IO
module Mat = Owl.Mat
type learning_metadata = {minimum : float; jacobian : IO.Mat.mat}
                         

let construct_fun_from_params params = fun input -> Mat.(((fst  params) *@ input) + (snd params))


let construct_cost data params =
  let f = construct_fun_from_params params in
  let residuals = Mat.(f  (fst data) - (snd data) ) in
  Mat.(transpose (residuals) *@ residuals)

let construct_deriv data params = 
    Mat.((fst data) @= (Mat.ones 1 1) )

let learn data params = (params, Some {minimum = 0.0; jacobian = (Mat.zeros 2 2)})

  

