open OUnit

  

exception IOFailure of string
    
let test_file = IO.(read "./test.csv" R)
let test_params_holder = IO.(make_blank_params_file "F_Params.csv" RW)
let test_pf = IO.(unpack_params test_params_holder)
let params =  match test_pf with
    | None -> IOFailure "No parameters where extracted from the csv" |> raise
    | Some pf -> !(pf.params)
  

let io_tests = [ 
  
]
