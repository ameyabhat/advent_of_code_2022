type directory = {
  dname: string;
  mutable children: directory list;
  parent: directory option;
  mutable files: file list;
}
and file = {
  fname: string;
  fsize: int;
}

let rec compute_node_size directory = 
  let self_size = (List.fold_right (fun f1 x -> f1.fsize + x) (directory.files) 0 ) in 
  let children_size = List.fold_right (+) (List.map compute_node_size directory.children) 0 in
  self_size + children_size

let insert_dir node dir = dir.children <- node :: dir.children
let insert_file fname fsize dir = dir.files <- {fname; fsize} :: dir.files
let new_directory dname parent = { dname; parent; children= []; files = []} 
  
let add_directory dname parent = 
  let new_dir = new_directory dname parent in 
  match parent with 
  | Some dir -> let () = insert_dir new_dir dir in
                new_dir
                
  | None -> new_dir

let find_dir dname current_dir = List.find (fun c_dir -> String.equal c_dir.dname dname) current_dir.children