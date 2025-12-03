open Base
open Stdio

let argv = Sys.get_argv ()
let infilename = argv.(1)

let banks = In_channel.read_lines infilename |>
  List.map ~f:(fun l -> 
               String.to_list l |>
               List.map ~f:(fun c -> String.of_char c |> Int.of_string))

let best_joltage_p1 bank = 
  let bank_sans_last = List.drop_last_exn bank in
  let maxd = List.max_elt bank_sans_last ~compare:Int.ascending |> Option.value_exn in
  let (i, _) = List.findi bank ~f:(fun _i a -> a = maxd) |> Option.value_exn in
  let banklen = List.length bank in 
  let rest = List.sub bank ~pos:(i + 1) ~len:(banklen - i - 1) in
  let maxs = List.max_elt rest ~compare:Int.ascending |> Option.value_exn in
  maxd * 10 + maxs

let () = List.map banks ~f:best_joltage_p1 |>
  List.fold ~init:0 ~f:(+) |>
  printf "p1 best joltage: %d\n"
