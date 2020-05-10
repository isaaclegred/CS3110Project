(** Functions for input and output of data and parameters.  Data is 
    distinguished by the fact that it should not be writable, only 
    readable, whereas parameters should be writable based on
    updates to them given by the trainer.
*)
(** The type of file permissions*)
type file_permission = R | RW

(** The type of independent_data *)
type independent_data = Owl.Mat.mat

(** The type of dependent_data *)
type dependent_data = Owl.Mat.mat

(** The type of weights *)                  
type weights = Owl.Mat.mat

(** The type of biases *)                
type biases = Owl.Mat.mat

(** The type of csv files *)
type csv = Csv.t

(** The type of files that hold data on which to test the net *)
type data_file = {
  path : string;
  status : file_permission;
  data : (independent_data * dependent_data) option;
  file : csv;
}

(** [read path status] returns a data file option with path [path], 
    status [status], data [None] and file a csv loaded from path [path]
    if such a csv exists at that path, otherwise return [None].
*)
val read : string -> file_permission -> data_file option

(** [mat_from_lst lst n] gets a matrix from a float list, so that the resulting 
    matrix has [n] rows. 
    
    Raises : [Incorrect_dimension] if the length of if the length of [lst] is 
    not divisible by [n].
*)
val mat_from_list : float list -> int -> Owl.Mat.mat

(** [unpack_data data_file] If data_file is not [None], and the data
    in the [csv] of [data_file] has not already been extracted, then
    this function processes float data contained in the csv in
    data_file and stores it in the [data] entry of the data_file record.
    Returning a data_file option.

    Requires : csv represented  by [data_file] satisfies the data format
    requirement. The first two lines of the file will be processed into
    [dependent_data] and [dependent_data], and each must start with a one
    word (one comma-separated value) identifier.

    Raises : [Failure] if the stored data doesn't adhere to the data layout
    requirements.
*)
val unpack_data : data_file option -> data_file option

(** The type of  parameters files, to be stored/extracted; [updated] is used to 
    flag when the parameters that were extracted from this file have been updated 
    so that the user can easily see if they need to be rewritten *)
type param_file = {
  path : string;
  status : file_permission;
  params : (weights * biases) list option ref;
  file : csv;
  updated : bool ref;
}

(** [process_params_into_mats path] processes the csv at location [path]   
    and returns a parameters list if it follows the storage format conventions
    
    Requires : file at [path] is a .csv file with 2*N lines for some N an 
    integer, and as the first line in each pair of lines has a length 
    (in terms of comma-separated values) which is divisble by the
    length of the next line.  The first in the pair will be processed into
    a row-filling-fastest matrix with a number of rows given by the length 
    of the next line, which will be processed into a column vector.  
    This convention is adhered to by [write_params]. 
*)
val process_params_into_mats :  string -> (weights * biases) list option
    
(** [unpack_params pf], if [pf] is not [None] takes the [path] of the 
    csv in [pf] and stores into the params entry of it the output of 
    [process_params_into_mats path].  Will not unpack if [updated] field
    is false, as the parameters haven't been updated.
*)
val unpack_params : param_file option -> param_file option

(** [write_params pf] writes the current parameters in the [params] filed of 
    [pf] to the .csv file whose path is contained in the record field [path]   
*)
val write_params : param_file -> param_file

(** [make_blank_parms_file path status] makes a params_file with [updated] false,
    and a ref None parameters.  
*)
val make_blank_params_file : string -> file_permission -> param_file
  
  
