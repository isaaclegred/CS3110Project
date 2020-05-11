(* At the top of your test file, which should be named test.ml or something
   very similar so that the grader can find it, please write a (potentially
   lengthy) comment describing your approach to testing: what you tested,
   anything you omitted testing, and why you believe that your test suite
   demonstrates the correctness of your system. *)

(*
We have implemented tests from two separte points of view, one from unit testing
contained in this file, and one from holistic testing, contained in learning.ml.
These tests are designed to test individual components and independent 
operations: things that may be be points of failure along the way to making and 
training a neural net.  These include IO and other low level module tests, but 
also tests of the network module functions.  At some point testing these 
functions requires constructing full networks, after this point much of
the testing is no longer "unit like" and becomes a full test of the system. 
*)
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
let test_pf = make_blank_params_file "test_params.csv" RW
let e_params = (Mat.of_array [|1.; 1.; 1.; 2.; 2.; 2.; 3.; 3.; 3.|] 3 3,
              Mat.of_array [|1.; 1.; 1.|] 3 1) 
let _ = update_params test_pf (Some [e_params])

let written_pf = write_params test_pf
let r_test_pf = make_blank_params_file "test_params.csv" RW
let r_params =
  match unpack_params (Some r_test_pf)  with
  | None -> IOFailure "No parameters were extracted from the csv" |> raise
  | Some pf -> match !(pf.params) with
    | None -> IOFailure "No parameters were extracted from the csv" |> raise
    | Some [] ->IOFailure "No parameters were extracted from the csv" |> raise
    | Some ((w,b)::t) -> (w,b) 
let io_tests = [
  cmp_mat_arr "independent_data test" ([| 1.; 2.; 3. |], 3, 1) (fst data);
  cmp_mat_arr "dependent_data test" ([|1.; 4.; 9. |], 3, 1) (snd data);
  cmp_mats "written and read weights" (fst e_params) (fst r_params);
  cmp_mats "written and read biases" (snd e_params) (snd r_params);
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
