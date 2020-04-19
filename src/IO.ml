module IO = struct
  type file_permission = R | RW
  module Mat = Owl.Mat
  (* independent data represents time steps the solution is evaluated at, must 
     be strictly increasing*)
  type independent_data = Mat.mat
  type dependent_data = Mat.mat
  type csv = Csv.t
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
  let process_into_mats c =
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
    | {path = p; status =f_p;
       data = Some d; file = c} as f -> let _ = print_endline ("data already extracted") in
      f
    | {path = p; status =f_p;
       data = None; file = c} -> let _ = print_endline ("extracting data") in
       let mat_data = process_into_mats p in  {path = p; status =f_p;
       data = mat_data; file = c}
    
end
