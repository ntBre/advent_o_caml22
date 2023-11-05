open Base
open Stdio

let load_file filename = In_channel.create filename |> In_channel.input_all

let _score1 s =
  match s with
  | "A X" -> 1 + 3
  | "A Y" -> 2 + 6
  | "A Z" -> 3 + 0
  | "B X" -> 1 + 0
  | "B Y" -> 2 + 3
  | "B Z" -> 3 + 6
  | "C X" -> 1 + 6
  | "C Y" -> 2 + 0
  | "C Z" -> 3 + 3
  | _ -> raise (Invalid_argument s)

let score2 s =
  match s with
  | "A X" -> 0 + 3
  | "A Y" -> 3 + 1
  | "A Z" -> 6 + 2
  | "B X" -> 0 + 1
  | "B Y" -> 3 + 2
  | "B Z" -> 6 + 3
  | "C X" -> 0 + 2
  | "C Y" -> 3 + 3
  | "C Z" -> 6 + 1
  | _ -> raise (Invalid_argument s)

let () =
  load_file "data/day02/input"
  |> String.split_lines |> List.map ~f:score2
  |> List.sum (module Int) ~f:Fn.id
  |> printf "%d\n"
