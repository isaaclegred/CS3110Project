(** Sample implementation of a neural network for demoing/testing. *)

(** [run_test input_size output_size iterations layer_num noise f] is a sample
    net with [layer_num] layers that takes in input of size [input_size], output
    of size [output_size], and has been trained [iterations] many times on data
    following a trend defined by [f] and fuzzed by a factor of [+/- noise]. *)
val run_test :
  int -> int -> int -> int -> float -> (float -> float) -> Network.net
