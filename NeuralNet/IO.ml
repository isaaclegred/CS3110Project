module Mat = Owl.Mat

type file_permission = R | RW
(* Independent data represents time steps the solution is evaluated at, must
   be strictly increasing *)
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
  file : csv
}

let read path status =
  try
    Some {path; status; data = None; file = Csv.load path}
  with Sys_error _ -> None

let mat_from_list lst =
  let mat = Mat.zeros 1 (List.length lst) in
  let _ =
    List.fold_left (
      fun idx elt -> let () = Mat.set mat 1 idx elt in idx + 1
    ) 0 lst in
  mat

(* TODO List.hd and List.tl are highly discouraged! *)
let process_data_into_mats c =
  let lists = Csv.load c in
  (* Require a single word identifier for the data in the csv such as ind, dep;
     but more generally the data being handled such as distance(m) or time(s) *)
  let ind =
    List.map (fun s -> float_of_string s) (List.tl (List.hd lists)) in
  let ind_mat = mat_from_list ind in
  let dep =
    List.map (fun s -> float_of_string s) (List.hd (List.tl (List.tl lists))) in
  let dep_mat = mat_from_list dep in
  Some (ind_mat, dep_mat)

let unpack_data data_file =
  match data_file with
  | None -> None
  | Some {path; status; data = Some _; file} as f ->
    let () = print_endline "data already extracted" in f
  | Some {path; status; data = None; file} ->
    let () = print_endline "extracting data" in
    let mat_data = process_data_into_mats path in
    Some {path; status; data = mat_data; file}

(* Will hold parameters, to be stored/extracted; [updated] is used to flag when
   the parameters that were extracted from this file have been updated so that
   the user can easily see if they need to be rewritten *)
type param_file = {
  path : string;
  status : file_permission;
  params : (weights * biases) list option;
  file : csv;
  updated : bool;
}

let process_params_into_mats c =
  let lists = Csv.load c in
  (* Need to be able to interpret the file in pairs as weights and biases *)
  if List.length lists mod 2 <> 0 then
    raise (Incorrect_Dimension ("in file " ^ c))
  else
    let rec process_helper lsts mats =
      match lsts with
      | [] -> []
      | weights :: biases :: t ->
        let w = List.map (fun s -> float_of_string s) (List.tl weights) in
        let b = List.map (fun s -> float_of_string s) (List.tl biases) in
        process_helper t ((mat_from_list w, mat_from_list b) :: mats)
      | unmatched :: t -> raise (Incorrect_Dimension ("in file " ^ c)) in
    Some (process_helper lists [])

let unpack_params params_file =
  match params_file with
  | None -> None
  | Some {path; status; params = Some _; file; updated = false} as f ->
    let () = print_endline "params already extracted" in f
  | Some {path; status; params = Some _; file; updated = true} ->
    let () = print_endline "overwriting updated params" in
    let mat_data = process_params_into_mats path in
    Some {path; status; params = mat_data; file; updated = false}
  | Some {path; status; params = None; file; updated} ->
    let () = print_endline "extracting params" in
    let mat_data = process_params_into_mats path in
    Some {path; status; params = mat_data; file; updated = false}

(* This function should unpack the parameters in the matrices and store them in
   the csv whose path is currently in the path slot. There needs to be some
   check to see if the file was opened with write permission, and there should
   also be a check to see if the data has been updated, becasue if not... *)
let write_params ({ path ;
  status;
  params;
  file ;
  updated;
} as params_file) =
  match updated with
  | false -> print_endline "writing parameters is unnecesary"; params_file
  | true ->
    match params with
    | None -> print_endline "no parameters to write"; params_file
    | Some p ->
    let row_to_list row = Mat.fold_cols (fun lst elt ->
      (string_of_float (Mat.get elt 0 0))::lst) [] row in
    let mat_to_csv mat = Mat.fold_rows (fun lst row ->
        ((row_to_list row)::lst)) []  mat in 
    let new_file  =
      List.fold_left (fun csv pair -> (fst pair |> mat_to_csv)@[]@
                                      (snd pair |> mat_to_csv)@[]::csv) [] p  in 
    Csv.save path new_file; {path; status; params; file = new_file; updated=false}


