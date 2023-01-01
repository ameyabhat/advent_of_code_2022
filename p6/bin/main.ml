open Core

let input = String.to_list (In_channel.read_all "input.txt")

let rec find_message (input_string : char list) (acc : int)
    (message_length : int) : int =
  if List.length input_string < message_length then failwith "no message found"
  else
    let char_set = Char.Set.of_list (List.take input_string message_length) in
    if Char.Set.length char_set = message_length then acc
    else
      find_message
        (Option.value_exn (List.tl input_string))
        (acc + 1) message_length

let () = printf "\n%d" (find_message input 4 4)
let () = printf "\n%d" (find_message input 14 14)
