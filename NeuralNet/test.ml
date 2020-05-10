open OUnit2
module Mat = Owl.Mat

let string_of_mat = Owl_pretty.dsnda_to_string
let cmp_mat_arr name (exp_arr, rows, columns) actual_mat : test =
  let exp_mat = Mat.of_array exp_arr rows columns in
  name >::(fun _ ->
      assert_equal exp_mat actual_mat ~printer:string_of_mat)
let cmp_mats name expected actual : test =
  name >::(fun _ ->
      assert_equal expected actual ~printer:string_of_mat)

(* IO Tests *)

open IO

exception IOFailure of string

let raw_test_file = read "./test.csv" R
let data_file = unpack_data raw_test_file
let data = match data_file with
  | None -> IOFailure "No data was extracted" |> raise
  | Some df -> match df.data with
    | None ->  IOFailure "No data was extracted" |> raise
    | Some local_data -> local_data
let test_params_holder = make_blank_params_file "F_Params.csv" RW
let test_pf = unpack_params (Some test_params_holder)
let params =
  match test_pf with
  | None -> IOFailure "No parameters were extracted from the csv" |> raise
  | Some pf -> !(pf.params)


  
let io_tests = [
cmp_mat_arr "independent_data test"  ([|1.; 2.; 3.|], 3, 1) (fst data)
]

(* Layer tests *)

let layer_tests = [

]

(* Network tests *)

let network_tests = [

]

(* Derivative tests *)

let derivative_tests = [

]

(* DefaultTrainer tests *)

let default_trainer_tests = [

]

(* Run tests *)

let tests = [
  io_tests;
  layer_tests;
  network_tests;
  derivative_tests;
  default_trainer_tests;
]

let suite = "project test suite" >::: List.flatten tests

let _ = run_test_tt_main suite
