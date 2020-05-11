(**
   We have implemented tests from two separate points of view, one from unit
   testing contained in this file, and one from holistic testing, contained in
   learning.ml. These tests are designed to test individual components and
   independent operations: things that may be points of failure along the way
   to making and training a neural net. These include IO and other low level
   module tests, but also tests of the Network module functions. At some point
   testing these functions requires constructing full networks, and after this
   point much of the testing is no longer "unit-like" and becomes a full test
   of the system.

   Our system has five main modules: Layer, Network, Derivative, DefaultTrainer
   and IO. We have extensively tested all five modules, but not all of them
   appear in this file because some of those modules do not lend themselves to
   OUnit testing. The modules Layer and Network were primarily tested by OUnit,
   while the rest of the modules were primarily manually tested. We describe
   our rationale below.

   - Layer and Network:
     We employed a mix of black box, glass box and randomized testing. Our
     tests are largely black box, because we tested the functions against the
     documented specification(s) in layer.mli and network.mli instead of the
     implementation(s). However, we needed to do some glass box testing for
     asserting that certain functions threw the right exceptions, because the
     specification does not say anything about the argument passed to the
     exception constructor. Thus, in order to use assert_raises correctly, we
     needed to refer to the implementation. Finally, we relied heavily on
     randomized testing to get a wide coverage of the space. To be specific, we
     tested the edge cases (n = 0, 1) by manually writing OUnit tests, since
     they have a higher chance of catastrophic behavior; the rest of the tests
     were randomized.

   - Derivative and DefaultTrainer:
     These are tested semi-manually in the Learning module. Additionally, that
     module also demonstrates an example of how to use our system. It is
     impossible to use OUnit tests to check these modules, because their
     workings involve a lot of opaque linear algebra and potential randomness,
     and so are not amenable to unit testing. Hence, we verify that these parts
     of the system work correctly by checking that our system manages to
     achieve its goal: to accurately predict data.

   - IO:
     To avoid cluttering the folder, the IO module was tested manually.
     Nevertheless, testing in utop demonstrates that it behaves correctly
     within its scope. It is, however, possible that IO will not work as
     intended on other OSes, etc; we only managed to test it on Macs.

   We believe our testing approach demonstrates the correctness of our system
   because it has been comprehensive. Most importantly, it is near impossible
   to formally verify that a neural network works correctly because the system
   is inherently opaque uses a degree of randomness. The large volume of
   randomized tests give us a good guarantee that our system will not crash,
   and the correctness of our neural network is demonstrated by the fact that
   it is able to achieve high accuracy when manually tested on random data sets.
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

(* IO tests *)

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
  match unpack_params (Some r_test_pf) with
  | None -> IOFailure "No parameters were extracted from the csv" |> raise
  | Some pf -> match !(pf.params) with
    | None -> IOFailure "No parameters were extracted from the csv" |> raise
    | Some [] -> IOFailure "No parameters were extracted from the csv" |> raise
    | Some ((w, b) :: t) -> (w, b)
let io_tests = [
  cmp_mat_arr "independent_data test" ([| 1.; 2.; 3. |], 3, 1) (fst data);
  cmp_mat_arr "dependent_data test" ([| 1.; 4.; 9. |], 3, 1) (snd data);
  cmp_mats "written and read weights" (fst e_params) (fst r_params);
  cmp_mats "written and read biases" (snd e_params) (snd r_params);
]

(* Layer tests *)

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
  "00 has zero activations" >:: (fun _ ->
      assert_equal 0 (Layer.get_activations layer_00 |> List.length));
  (* 01 getters *)
  "01 has input size 1" >:: (fun _ ->
      assert_equal 1 (Layer.input_size layer_01));
  "01 has layer size 0" >:: (fun _ ->
      assert_equal 0 (Layer.layer_size layer_01));
  "01 has 01 weights" >:: (fun _ ->
      assert_equal (Mat.create 0 1 0.) (Layer.get_weights layer_01));
  "01 has 11 biases" >:: (fun _ ->
      assert_equal (Mat.create 1 1 3110.) (Layer.get_biases layer_01));
  "01 has one activation" >:: (fun _ ->
      assert_equal 1 (Layer.get_activations layer_01 |> List.length));
  (* 02 getters *)
  "02 has input size 2" >:: (fun _ ->
      assert_equal 2 (Layer.input_size layer_02));
  "02 has layer size 0" >:: (fun _ ->
      assert_equal 0 (Layer.layer_size layer_02));
  "02 has 02 weights" >:: (fun _ ->
      assert_equal (Mat.create 0 2 0.) (Layer.get_weights layer_02));
  "02 has 21 biases" >:: (fun _ ->
      assert_equal (Mat.create 2 1 3110.) (Layer.get_biases layer_02));
  "02 has two activations" >:: (fun _ ->
      assert_equal 2 (Layer.get_activations layer_02 |> List.length));
  (* 10 getters *)
  "10 has input size 0" >:: (fun _ ->
      assert_equal 0 (Layer.input_size layer_10));
  "10 has layer size 1" >:: (fun _ ->
      assert_equal 1 (Layer.layer_size layer_10));
  "10 has 10 weights" >:: (fun _ ->
      assert_equal (Mat.create 1 0 0.) (Layer.get_weights layer_10));
  "10 has 01 biases" >:: (fun _ ->
      assert_equal (Mat.create 0 1 0.) (Layer.get_biases layer_10));
  "10 has zero activations" >:: (fun _ ->
      assert_equal 0 (Layer.get_activations layer_10 |> List.length));
  (* 11 getters *)
  "11 has input size 1" >:: (fun _ ->
      assert_equal 1 (Layer.input_size layer_11));
  "11 has layer size 1" >:: (fun _ ->
      assert_equal 1 (Layer.layer_size layer_11));
  "11 has 11 weights" >:: (fun _ ->
      assert_equal (Mat.create 1 1 3110.) (Layer.get_weights layer_11));
  "11 has 11 biases" >:: (fun _ ->
      assert_equal (Mat.create 1 1 3110.) (Layer.get_biases layer_11));
  "11 has one activation" >:: (fun _ ->
      assert_equal 1 (Layer.get_activations layer_11 |> List.length));
  (* 12 getters *)
  "12 has input size 2" >:: (fun _ ->
      assert_equal 2 (Layer.input_size layer_12));
  "12 has layer size 1" >:: (fun _ ->
      assert_equal 1 (Layer.layer_size layer_12));
  "12 has 12 weights" >:: (fun _ ->
      assert_equal (Mat.create 1 2 3110.) (Layer.get_weights layer_12));
  "12 has 21 biases" >:: (fun _ ->
      assert_equal (Mat.create 2 1 3110.) (Layer.get_biases layer_12));
  "12 has two activations" >:: (fun _ ->
      assert_equal 2 (Layer.get_activations layer_12 |> List.length));
  (* 20 getters *)
  "20 has input size 0" >:: (fun _ ->
      assert_equal 0 (Layer.input_size layer_20));
  "20 has layer size 2" >:: (fun _ ->
      assert_equal 2 (Layer.layer_size layer_20));
  "20 has 20 weights" >:: (fun _ ->
      assert_equal (Mat.create 2 0 0.) (Layer.get_weights layer_20));
  "20 has 01 biases" >:: (fun _ ->
      assert_equal (Mat.create 0 1 0.) (Layer.get_biases layer_20));
  "20 has zero activations" >:: (fun _ ->
      assert_equal 0 (Layer.get_activations layer_20 |> List.length));
  (* 21 getters *)
  "21 has input size 1" >:: (fun _ ->
      assert_equal 1 (Layer.input_size layer_21));
  "21 has layer size 2" >:: (fun _ ->
      assert_equal 2 (Layer.layer_size layer_21));
  "21 has 21 weights" >:: (fun _ ->
      assert_equal (Mat.create 2 1 3110.) (Layer.get_weights layer_21));
  "21 has 11 biases" >:: (fun _ ->
      assert_equal (Mat.create 1 1 3110.) (Layer.get_biases layer_21));
  "21 has one activation" >:: (fun _ ->
      assert_equal 1 (Layer.get_activations layer_21 |> List.length));
  (* 22 getters *)
  "22 has input size 2" >:: (fun _ ->
      assert_equal 2 (Layer.input_size layer_22));
  "22 has layer size 2" >:: (fun _ ->
      assert_equal 2 (Layer.layer_size layer_22));
  "22 has 22 weights" >:: (fun _ ->
      assert_equal (Mat.create 2 2 3110.) (Layer.get_weights layer_22));
  "22 has 21 biases" >:: (fun _ ->
      assert_equal (Mat.create 2 1 3110.) (Layer.get_biases layer_22));
  "22 has two activations" >:: (fun _ ->
      assert_equal 2 (Layer.get_activations layer_22 |> List.length));
  (* Test exceptions *)
  "Bad biases length" >:: (fun _ ->
      assert_raises (Invalid_argument "Shapes do not match")
        (fun () -> Layer.create mat_12 mat_11 [f; f] [f'; f']));
  "Bad biases length and biases not a column vector" >:: (fun _ ->
      assert_raises (Invalid_argument "Shapes do not match")
        (fun () -> Layer.create mat_12 mat_12 [f; f] [f'; f']));
  "Biases not a column vector" >:: (fun _ ->
      assert_raises (Invalid_argument "Shapes do not match")
        (fun () -> Layer.create mat_12 mat_22 [f; f] [f'; f']));
  "Bad acts and act_derivs length" >:: (fun _ ->
      assert_raises (Invalid_argument "Shapes do not match")
        (fun () -> Layer.create mat_12 mat_21 [f] [f']));
  "Mismatched acts and act_derivs length" >:: (fun _ ->
      assert_raises (Invalid_argument "Shapes do not match")
        (fun () -> Layer.create mat_12 mat_21 [f] [f'; f']));
]

let random_layer_test i =
  let m = Random.int (i + 1) + 1 in
  let n = Random.int (i + 1) + 1 in
  let weights = Mat.uniform m n in
  let biases = Mat.uniform n 1 in
  let acts = List.init n (fun _ x -> x) in
  let act_derivs = List.init n (fun _ _ -> 1.) in
  let layer = Layer.create weights biases acts act_derivs in
  let new_weights = Mat.uniform m n in
  let bad_weights = Mat.uniform (m + 1) (n + 1) in
  let new_biases = Mat.uniform n 1 in
  let bad_biases = Mat.uniform n 2 in
  let bad_acts = List.init (n + 1) (fun _ x -> x) in
  let bad_act_derivs = List.init (n + 1) (fun _ _ -> 1.) in
  [
    "input_size" >:: (fun _ ->
        assert_equal n (Layer.input_size layer));
    "layer_size" >:: (fun _ ->
        assert_equal m (Layer.layer_size layer));
    "get_weights" >:: (fun _ ->
        assert_equal weights (Layer.get_weights layer));
    "set_weights" >:: (fun _ ->
        assert_equal
          new_weights
          (layer |> Layer.set_weights new_weights |> Layer.get_weights));
    "set_weights fail" >:: (fun _ ->
        assert_raises
          (Invalid_argument "Bad shape weights")
          (fun () -> layer |> Layer.set_weights bad_weights));
    "incr_weights" >:: (fun _ ->
        assert_equal
          Mat.(weights + new_weights)
          (layer |> Layer.incr_weights new_weights |> Layer.get_weights));
    "incr_weights fail" >:: (fun _ ->
        assert_raises
          (Invalid_argument "Bad shape weights")
          (fun () -> layer |> Layer.incr_weights bad_weights));
    "decr_weights" >:: (fun _ ->
        assert_equal
          Mat.(weights - new_weights)
          (layer |> Layer.decr_weights new_weights |> Layer.get_weights));
    "decr_weights fail" >:: (fun _ ->
        assert_raises
          (Invalid_argument "Bad shape weights")
          (fun () -> layer |> Layer.decr_weights bad_weights));
    "get_biases" >:: (fun _ ->
        assert_equal biases (Layer.get_biases layer));
    "set_biases" >:: (fun _ ->
        assert_equal
          new_biases
          (layer |> Layer.set_biases new_biases |> Layer.get_biases));
    "set_biases fail" >:: (fun _ ->
        assert_raises
          (Invalid_argument "Bad shape biases")
          (fun () -> layer |> Layer.set_biases bad_biases));
    "incr_biases" >:: (fun _ ->
        assert_equal
          Mat.(biases + new_biases)
          (layer |> Layer.incr_biases new_biases |> Layer.get_biases));
    "incr_biases fail" >:: (fun _ ->
        assert_raises
          (Invalid_argument "Bad shape biases")
          (fun () -> layer |> Layer.incr_biases bad_biases));
    "decr_biases" >:: (fun _ ->
        assert_equal
          Mat.(biases - new_biases)
          (layer |> Layer.decr_biases new_biases |> Layer.get_biases));
    "decr_biases fail" >:: (fun _ ->
        assert_raises
          (Invalid_argument "Bad shape biases")
          (fun () -> layer |> Layer.decr_biases bad_biases));
    "get_activations" >:: (fun _ ->
        assert_equal n
          (layer |> Layer.get_activations |> List.length));
    "set_activations" >:: (fun _ ->
        assert_equal n
          (layer
           |> Layer.set_activations acts act_derivs
           |> Layer.get_activations
           |> List.length));
    "set_activations fail" >:: (fun _ ->
        assert_raises
          (Invalid_argument "Shapes do not match")
          (fun () -> layer |> Layer.set_activations bad_acts bad_act_derivs));
    "copy not physically equal" >:: (fun _ ->
        assert_bool "copy !=" (layer != Layer.copy layer));
  ]

let random_layer_tests n =
  random_layer_test |> List.init n |> List.flatten

(* Network tests *)

let pre_net_01 = Network.create 0 1
let pre_net_10 = Network.create 1 0

let network_tests = [
  "create fail in" >:: (fun _ ->
      assert_raises
        (Invalid_argument "Sizes cannot be negative")
        (fun () -> Network.create (-1) 1));
  "create fail out" >:: (fun _ ->
      assert_raises
        (Invalid_argument "Sizes cannot be negative")
        (fun () -> Network.create 1 (-1)));
  "create fail in and out" >:: (fun _ ->
      assert_raises
        (Invalid_argument "Sizes cannot be negative")
        (fun () -> Network.create (-1) (-1)));
  "seal fail a" >:: (fun _ ->
      assert_raises
        (Invalid_argument "Shapes do not match")
        (fun () -> Network.seal pre_net_01));
  "seal fail b" >:: (fun _ ->
      assert_raises
        (Invalid_argument "Shapes do not match")
        (fun () -> Network.seal pre_net_10));
]

let random_network_test i =

  let i_size = Random.int (i + 1) + 1 in
  let o_size = Random.int (i + 1) + 1 in
  let layer_num = Random.int (i + 1) + 1 in
  let max_size = Random.int (i + 1) + 1 in

  let layer_sizes = Array.init layer_num (fun i ->
      if i = layer_num - 1 then o_size else Random.int max_size + 1
    ) in

  let layers = Array.init layer_num (fun i ->
      let input_size = if i = 0 then i_size else layer_sizes.(i - 1) in
      Layer.create
        (Mat.uniform layer_sizes.(i) input_size)
        (Mat.zeros input_size 1)
        (List.init input_size (fun _ x -> x))
        (List.init input_size (fun _ _ -> 1.))
    ) in

  let pre_net =
    Network.create i_size o_size
    |> (fun x -> Array.fold_left (fun pre_net layer ->
        Network.add_layer layer pre_net) x layers) in

  let network = pre_net |> Network.seal in

  [
    "input_size" >:: (fun _ ->
        assert_equal i_size (Network.input_size network));
    "output_size" >:: (fun _ ->
        assert_equal o_size (Network.output_size network));
    "pre_net_layers" >:: (fun _ ->
        assert_equal
          (layers |> Array.length)
          (pre_net |> Network.pre_net_layers |> Array.length));
    "net_layers" >:: (fun _ ->
        assert_equal
          (layers |> Array.length)
          (network |> Network.net_layers |> Array.length));
    "copy_pre_net not physically equal" >:: (fun _ ->
        assert_bool "copy !=" (pre_net != Network.copy_pre_net pre_net));
    "copy_net not physically equal" >:: (fun _ ->
        assert_bool "copy !=" (network != Network.copy_net network));
    "to_parameter_list" >:: (fun _ ->
        assert_equal
          (List.init layer_num (fun i ->
               let layer = layers.(i) in
               Layer.get_weights layer, Layer.get_biases layer))
          (Network.to_parameter_list network));
    "from_parameter_list" >:: (fun _ ->
        assert_equal
          (network
           |> Network.to_parameter_list)
          (network
           |> Network.to_parameter_list
           |> Network.from_parameter_list
           |> Network.to_parameter_list));
  ]

let random_network_tests n =
  random_network_test |> List.init n |> List.flatten

(* Run tests *)

let tests = [
  io_tests;
  layer_tests;
  random_layer_tests 100;
  network_tests;
  random_network_tests 100;
]

let suite = "project test suite" >::: List.flatten tests

let _ = run_test_tt_main suite
