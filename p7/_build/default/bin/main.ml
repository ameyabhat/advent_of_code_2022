(* Two types - directory, and file *)
(* File is the leaf of our tree *)
(* A directory should have a reference to it's parent - this should be an option, just in case it's the root *)
(* But these both need to be the same type*)

(* I need to construct the file tree, and then find all directories with size < 10k *)
(* We can count files more than once! *)

open Core
open Ftree.File_tree

let input =
  List.map ~f:String.strip
    (List.filter
       (String.split (In_channel.read_all "input.txt") ~on:'$')
       ~f:(fun a -> not (String.is_empty a)))

let pruned_input = List.sub input ~pos:1 ~len:(List.length input - 1)
let root = new_directory "/" None

let execute_command (dir : directory) (command : string) : directory =
  let handle_ls ls_command =
    match String.split ls_command ~on:' ' with
    | [ "dir"; dname ] ->
        let _ = add_directory dname (Some dir) in
        ()
    | [ fsize; fname ] -> insert_file fname (Int.of_string fsize) dir
    | _ -> failwith "unexpected input to ls"
  in
  match Str.string_before command 2 with
  | "cd" -> (
      match List.nth_exn (String.split command ~on:' ') 1 with
      | ".." -> Option.value_exn dir.parent
      | dname -> find_dir dname dir)
  | "ls" ->
      let ls_seqs = String.split ~on:'\n' command in
      let ls_commands =
        List.sub ls_seqs ~pos:1 ~len:(List.length ls_seqs - 1)
      in
      let () = List.iter ls_commands ~f:handle_ls in
      dir
  | _ -> failwith "command not recognized"

let _ = List.fold_left ~init:root ~f:execute_command pruned_input

let rec flatten_tree_size dir =
  compute_node_size dir :: List.concat_map dir.children ~f:flatten_tree_size

let soln =
  root |> flatten_tree_size
  |> List.filter ~f:(fun dsize -> dsize < 100000)
  |> List.fold_right ~f:( + ) ~init:0

let root_size = compute_node_size root
let unused_space = 70000000 - root_size
let space_required = 30000000 - unused_space
let () = printf "%d\n" root_size

let soln2 =
  root |> flatten_tree_size
  |> List.filter ~f:(fun dsize -> dsize > space_required)
  |> List.fold_right ~f:min ~init:root_size

let () = printf "%d\n" soln
let () = printf "%d\n" soln2
