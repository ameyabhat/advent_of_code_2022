open Core

let input = In_channel.read_lines "input.txt"

let scores =
  Char.Table.of_alist_exn
    [
      ('a', 1);
      ('b', 2);
      ('c', 3);
      ('d', 4);
      ('e', 5);
      ('f', 6);
      ('g', 7);
      ('h', 8);
      ('i', 9);
      ('j', 10);
      ('k', 11);
      ('l', 12);
      ('m', 13);
      ('n', 14);
      ('o', 15);
      ('p', 16);
      ('q', 17);
      ('r', 18);
      ('s', 19);
      ('t', 20);
      ('u', 21);
      ('v', 22);
      ('w', 23);
      ('x', 24);
      ('y', 25);
      ('z', 26);
      ('A', 27);
      ('B', 28);
      ('C', 29);
      ('D', 30);
      ('E', 31);
      ('F', 32);
      ('G', 33);
      ('H', 34);
      ('I', 35);
      ('J', 36);
      ('K', 37);
      ('L', 38);
      ('M', 39);
      ('N', 40);
      ('O', 41);
      ('P', 42);
      ('Q', 43);
      ('R', 44);
      ('S', 45);
      ('T', 46);
      ('U', 47);
      ('V', 48);
      ('W', 49);
      ('X', 50);
      ('Y', 51);
      ('Z', 52);
    ]

let rec find_passes = function
  | [] -> []
  | a :: b :: c :: t ->
      let set_a = Char.Set.of_list (String.to_list a) in
      let set_b = Char.Set.of_list (String.to_list b) in
      let set_c = Char.Set.of_list (String.to_list c) in
      let pass =
        Option.value
          (List.nth
             (Char.Set.elements
                (Char.Set.inter (Char.Set.inter set_a set_b) set_c))
             0)
          ~default:Char.max_value
      in
      pass :: find_passes t
  | _ -> failwith "stop bothering me"

let intersecting_char str_input =
  let first_half =
    String.sub str_input ~pos:0 ~len:(String.length str_input / 2)
  in
  let first_set = Char.Set.of_list (String.to_list first_half) in

  let second_half =
    String.sub str_input
      ~pos:(String.length str_input / 2)
      ~len:(String.length str_input / 2)
  in
  let second_set = Char.Set.of_list (String.to_list second_half) in

  match
    List.nth (Char.Set.elements (Char.Set.inter first_set second_set)) 0
  with
  | Some v -> v
  | None -> failwith "didn't find an intersection"

let compute_scores overlapping_char =
  match Char.Table.find scores overlapping_char with
  | Some x -> x
  | None -> failwith "Not an [a-z][A-Z]"

let _ =
  input
  |> List.map ~f:intersecting_char
  |> List.map ~f:compute_scores
  |> List.fold_left ~init:0 ~f:( + )

let passScore =
  input |> find_passes |> List.map ~f:compute_scores
  |> List.fold_left ~init:0 ~f:( + )

let () = printf "%d" passScore
