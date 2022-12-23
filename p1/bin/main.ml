open Printf

let read_input fname =
  let ch = open_in fname in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let input = read_input "p1_input.txt"
let grouped_cals = Str.split (Str.regexp "\n\n") input
let all_split = List.map (Str.split (Str.regexp "\n")) grouped_cals
let to_numbers = List.map (List.map int_of_string) all_split
let sums = List.map (List.fold_left ( + ) 0) to_numbers
let sorted_sums = List.sort (fun x y -> ~-(compare x y)) sums

let rec slice b e l =
  match l with
  | [] -> failwith "sublist"
  | h :: t ->
      let tail = if e == 0 then [] else slice (b - 1) (e - 1) t in
      if b > 0 then tail else h :: tail

let sum_of_three = List.fold_right ( + ) (slice 0 2 sorted_sums) 0
let () = printf "%d\n" sum_of_three
