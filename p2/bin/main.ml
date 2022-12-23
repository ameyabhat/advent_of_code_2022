open Core
open Printf

let read_input file = In_channel.read_lines file
let input = read_input "input2.txt"

(* Opp plays A *)
let opp_plays_a = [ ("X", 3); ("Y", 4); ("Z", 8) ]
let opp_plays_b = [ ("X", 1); ("Y", 5); ("Z", 9) ]
let opp_plays_c = [ ("X", 2); ("Y", 6); ("Z", 7) ]

let strategy_map =
  [ ("A", opp_plays_a); ("B", opp_plays_b); ("C", opp_plays_c) ]

let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if String.equal k k' then Some v else lookup k t

let score_round opp_play play =
  match lookup opp_play strategy_map with
  | Some strategy -> (
      match lookup play strategy with
      | Some score -> score
      | None -> failwith "inner failure")
  | None -> failwith "outer failure"

let score_round_aux (input_list : string list) =
  let opp_play = Option.value (List.nth input_list 0) ~default:"" in
  let play = Option.value (List.nth input_list 1) ~default:"" in
  score_round opp_play play

let soln =
  List.fold_right ~f:( + ) ~init:0
    (List.map ~f:score_round_aux
       (List.map ~f:(Str.split (Str.regexp " ")) input))

let () = printf "%d" soln
