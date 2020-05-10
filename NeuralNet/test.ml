(* At the top of your test file, which should be named test.ml or something
   very similar so that the grader can find it, please write a (potentially
   lengthy) comment describing your approach to testing: what you tested,
   anything you omitted testing, and why you believe that your test suite
   demonstrates the correctness of your system. *)

open OUnit2

(* IO Tests *)

open IO

exception IOFailure of string

let test_file = read "./test.csv" R
let test_params_holder = make_blank_params_file "F_Params.csv" RW
let test_pf = unpack_params (Some test_params_holder)
let params =
  match test_pf with
  | None -> IOFailure "No parameters were extracted from the csv" |> raise
  | Some pf -> !(pf.params)

let io_tests = [

]

(* Layer tests *)

let layer_tests = [
  "The empty dict has size 0" >:: (fun _ ->
      assert_equal 0 (D.size empty));
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
