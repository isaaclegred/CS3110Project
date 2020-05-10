module Mat = Owl.Mat

type file_permission = R | RW
type independent_data = Mat.mat
type dependent_data = Mat.mat
(* Params can be either weights or biases *)
type weights = Mat.mat
type biases = Mat.mat
(* Wrapping the csv file type, it is really just a list of string lists, but
   just in case we come up with a better data format *)
type csv = Csv.t

exception Incorrect_Dimension of string

type data_file = {
  path : string;
  status : file_permission;
  data : (independent_data * dependent_data) option;
  file : csv;
}

let read path status =
  try
    Some {path; status; data = None; file = Csv.load path}
  with Sys_error _ -> None

let mat_from_list lst n =
  let index_fun max n id =
    if max = n then id, 0 else id / (max / n), id mod (max / n) in
  if List.length lst mod n <> 0 then
    let err = "This list cannot be split into " ^ string_of_int n ^ " rows" in
    raise (Incorrect_Dimension err)
  else
    let max = List.length lst in
    let mat = Mat.zeros n (max / n) in
    let f idx elt =
      let x, y = index_fun max n idx in
      Mat.set mat x y elt;
      idx + 1 in
    let _ = List.fold_left f 0 lst in
    mat

let hd default = function
  | [] -> default
  | h :: t -> h

let tl = function
  | [] -> []
  | h :: t -> t

let unpack_data data_file =
  let process_data_into_mats c =
    let lists = Csv.load c in
    (* Need a single word identifier for the data in the csv such as ind, dep;
       but generally the data being handled such as distance(m) or time(s). *)
    let ind = List.map float_of_string (tl (hd [] lists)) in
    let ind_n = List.length ind in
    let ind_mat = mat_from_list ind ind_n in
    let dep = List.map float_of_string (hd [] (tl (tl lists))) in
    let dep_n = List.length dep in
    let dep_mat = mat_from_list dep dep_n in
    Some (ind_mat, dep_mat) in
  match data_file with
  | None -> None
  | Some {path; status; data = Some _; file} as f ->
    print_endline "data already extracted"; f
  | Some {path; status; data = None; file} ->
    print_endline "extracting data";
    let mat_data = process_data_into_mats path in
    Some {path; status; data = mat_data; file}

type param_file = {
  path : string;
  status : file_permission;
  params : (weights * biases) list option ref;
  file : csv;
  updated : bool ref;
}

let process_params_into_mats c =
  let lists = Csv.load c in
  (* Need to be able to interpret the file in pairs as weights and biases *)
  if List.length lists mod 2 <> 0 then
    raise (Incorrect_Dimension ("in file " ^ c))
  else
    let rec process_helper lsts mats =
      match lsts with
      | [] -> mats
      | weights :: biases :: t ->
        (* We can use the length of the biases to infer the number of rows in
           the weights matrix *)
        let n = List.length biases in
        let w = List.map float_of_string weights in
        let b = List.map float_of_string biases in
        process_helper t ((mat_from_list w n, mat_from_list b n) :: mats)
      | unmatched :: t -> raise (Incorrect_Dimension ("in file " ^ c)) in
    Some (process_helper lists [])

let unpack_params params_file =
  match params_file with
  | None -> None
  | Some f when !(f.params) <> None && not !(f.updated) ->
    print_endline "params already extracted"; Some f
  | Some {path; status; params; file; updated}
    when !params <> None && !updated ->
    print_endline "overwriting updated params";
    params := process_params_into_mats path;
    Some {path; status; params; file; updated = ref false}
  | Some {path; status; params; file; updated} when !params = None ->
    print_endline "extracting params";
    params := process_params_into_mats path;
    Some {path; status; params; file; updated = ref false}
  | f -> print_endline "undefined behavior"; f

(* This function should unpack the parameters in the matrices and store them in
   the csv whose path is currently in the path slot. There needs to be some
   check to see if the file was opened with write permission, and there should
   also be a check to see if the data has been updated, because if not... *)
let write_params ({path; status; params; file; updated} as params_file) =
  match !updated with
  | false -> print_endline "writing parameters is unnecesary"; params_file
  | true ->
    begin match !params with
      | None -> print_endline "no parameters to write"; params_file
      | Some p ->
        let f lst mat = string_of_float (Mat.get mat 0 0) :: lst in
        let column_to_csv col = Mat.fold_rows f [] col in
        let row_to_list row = Mat.fold_cols f [] row in
        let g lst row = row_to_list row @ lst in
        let mat_to_csv mat = Mat.fold_rows g [] mat in
        let h csv (x, y) = mat_to_csv x :: column_to_csv y :: csv in
        let new_file = List.fold_left h [] p in
        Csv.save path new_file;
        {path; status; params; file = new_file; updated = ref false}
    end

let make_blank_params_file path status =
  {path; status; params = ref None; file = []; updated = ref false}
