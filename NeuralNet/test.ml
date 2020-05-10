(* At the top of your test file, which should be named test.ml or something
   very similar so that the grader can find it, please write a (potentially
   lengthy) comment describing your approach to testing: what you tested,
   anything you omitted testing, and why you believe that your test suite
   demonstrates the correctness of your system. *)

open OUnit2
module Mat = Owl.Mat

let string_of_mat = Owl_pretty.dsnda_to_string
let cmp_mat_arr name (exp_arr, rows, columns) actual_mat : test =
  let exp_mat = Mat.of_array exp_arr rows columns in
  name >:: (fun _ ->
      assert_equal exp_mat actual_mat ~printer:string_of_mat)
let cmp_mats name expected actual : test =
  name >:: (fun _ ->
      assert_equal expected actual ~printer:string_of_mat)

(* IO Tests *)

open IO

exception IOFailure of string

let raw_test_file = read "./test.csv" R
let data_file = unpack_data raw_test_file
let data = match data_file with
  | None -> IOFailure "No data was extracted" |> raise
  | Some df ->
    begin match df.data with
      | None -> IOFailure "No data was extracted" |> raise
      | Some local_data -> local_data
    end
let test_params_holder = make_blank_params_file "F_Params.csv" RW
let test_pf = unpack_params (Some test_params_holder)
let params =
  match test_pf with
  | None -> IOFailure "No parameters were extracted from the csv" |> raise
  | Some pf -> !(pf.params)

let io_tests = [
  cmp_mat_arr "independent_data test" ([| 1.; 2.; 3. |], 3, 1) (fst data)
]

(* Layer tests *)

open Layer

let mat_00 = Mat.create 0 0 3110.
let mat_01 = Mat.create 0 1 3110.
let mat_02 = Mat.create 0 2 3110.
let mat_10 = Mat.create 1 0 3110.
let mat_11 = Mat.create 1 1 3110.
let mat_12 = Mat.create 1 2 3110.
let mat_20 = Mat.create 2 0 3110.
let mat_21 = Mat.create 2 1 3110.
let mat_22 = Mat.create 2 2 3110.

let f x = x
let f' _ = 1.

let layer_00 = Layer.create mat_00 mat_01 [] []
let layer_01 = Layer.create mat_01 mat_11 [f] [f']
let layer_02 = Layer.create mat_02 mat_21 [f; f] [f'; f']
let layer_10 = Layer.create mat_10 mat_01 [] []
let layer_11 = Layer.create mat_11 mat_11 [f] [f']
let layer_12 = Layer.create mat_12 mat_21 [f; f] [f'; f']
let layer_20 = Layer.create mat_20 mat_01 [] []
let layer_21 = Layer.create mat_21 mat_11 [f] [f']
let layer_22 = Layer.create mat_22 mat_21 [f; f] [f'; f']

let layer_tests = [
  (* 00 getters *)
  "00 has input size 0" >:: (fun _ ->
      assert_equal 0 (Layer.input_size layer_00));
  "00 has layer size 0" >:: (fun _ ->
      assert_equal 0 (Layer.layer_size layer_00));
  "00 has 00 weights" >:: (fun _ ->
      assert_equal (Mat.create 0 0 0.) (Layer.get_weights layer_00));
  "00 has 01 biases" >:: (fun _ ->
      assert_equal (Mat.create 0 1 0.) (Layer.get_biases layer_00));
  "00 has empty activations" >:: (fun _ ->
      assert_equal [] (Layer.get_activations layer_00));
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
