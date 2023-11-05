open Base
open Stdio

let load_file filename = In_channel.create filename |> In_channel.input_all

let get_elves lst =
  List.fold lst ~init:[] ~f:(fun acc elt ->
      if String.( = ) elt "" then 0 :: acc
      else
        let i = Int.of_string elt in
        match acc with [] -> [ i ] | hd :: tl -> (hd + i) :: tl)

let () =
  let take n l = List.take l n in
  load_file "input" |> String.split ~on:'\n' |> get_elves
  |> List.sort ~compare:Int.descending
  |> take 3
  |> List.sum (module Int) ~f:Fn.id
  |> printf "%d\n"
