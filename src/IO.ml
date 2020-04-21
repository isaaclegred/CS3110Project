module IO = struct
  type file_permission = R | RW
  module Mat = Owl.Mat
  (* independent data represents time steps the solution is evaluated at, must 
     be strictly increasing*)
  type independent_data = Mat.mat
  type dependent_data = Mat.mat
  (* Params can be either weights or biases *)                    
  type weights = Mat.mat
  type biases = Mat.mat
  (* Wrapping the csv file type, it is really just a list of string lists, but just in case 
     we come up with a better data format*)
  type csv = Csv.t
  
  exception Incorrect_Dimension of string
  type data_file  = {path : string; status : file_permission;
                     data : (independent_data * dependent_data) option; file : csv}
  let read csv_path file_permission =  try Some ({path = csv_path;
                                             status=file_permission;
                                             data= None;
                                             file = Csv.load csv_path})
    with Sys_error e ->
      None

  let mat_from_list lst =
    let mat = Mat.zeros 1 (List.length lst) in 
    let _ = List.fold_left (fun idx elt -> let _ = Mat.set mat 1 idx elt in (idx + 1)) 0 lst in
    mat
  let process_data_into_mats c =
    let lists = Csv.load c in
    (* Require a single word identifier for the data in the csv such as ind, dep,
       but more generally the data being handled such as distance(m) or time(s)*)
    let ind = List.map (fun s -> float_of_string s)
        (List.tl (List.hd lists)) in 
    let ind_mat = mat_from_list ind in
    let dep = List.map (fun s -> float_of_string s)
        (List.hd (List.tl (List.tl lists))) in
    let dep_mat = mat_from_list dep in
    Some (ind_mat, dep_mat)
    
    
    
      
  let unpack_data data_file =
    match data_file with
    | None -> None
    | Some {path = p; status =f_p;
       data = Some d; file = c} as f -> let _ = print_endline ("data already extracted") in
      f
    | Some {path = p; status =f_p;
       data = None; file = c} -> let _ = print_endline ("extracting data") in
       let mat_data = process_data_into_mats p in  Some {path = p; status =f_p;
                                               data = mat_data; file = c}
                                                   
  (* Will hold parameters, to be stored/ extracted, updated is used to flag when the
     parameters that were extracted from this file have been updated so that the user
     can easily see if they need to be rewritten*)
  type param_file = {path : string; status : file_permission;
                     params : (weights * biases) list option; file : csv;
                    updated : bool}

  let process_params_into_mats c =
    let lists = Csv.load c in
    (* need to be able to interpret the file in pairs as weights and biases *)
    if List.length lists mod 2 <> 0
    then
      let _ = raise (Incorrect_Dimension ("in file "^ c)) in None
    else
      let rec process_helper lsts mats =
        match lsts with
        | []  -> []
        | weights::biases::t ->
          let w = List.map (fun s -> float_of_string s)
              (List.tl weights) in
          let b = List.map (fun s -> float_of_string s)
              (List.tl biases) in
          process_helper t ((mat_from_list w , mat_from_list b)::mats)
        | unmatched::t -> let _ = raise (Incorrect_Dimension ("in file "^ c)) in []
            in Some (process_helper lists [])

  
  let unpack_params params_file =
    match params_file with
    | None -> None
    | Some {path = p; status =f_p;
       params = Some d; file = c;
       updated = false} as f -> let _ = print_endline ("params already extracted") in
      f
    | Some {path = p; status =f_p;
       params = Some d; file = c;
       updated = true} -> let _  = print_endline ("overwriting updated params") in
      let mat_data = process_params_into_mats p in  Some {path = p; status =f_p;
                                                          params = mat_data; file = c;
                                                          updated=false }
    | Some {path = p; status =f_p;
       params = None; file = c; updated=t_f} -> let _ = print_endline ("extracting params") in
      let mat_data = process_params_into_mats p in  Some {path = p; status =f_p;
                                                      params = mat_data; file = c; updated=false }
                                                     
  (* This function should unpack the parameters in the matrices and store than in the 
  csv that has it's path currently being stored in the path slot, there needs to be
  some check to see if the file was opened with write permission, and probably there 
  should also be a check to see if the data has been updated, becasue if not*)
  let write_params params_file =
    failwith "unimplemented"
    
end
