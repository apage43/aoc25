open Base
open Stdio

let argv = Sys.get_argv ()
let infilename = argv.(1)

let rtiles = In_channel.read_lines infilename |> 
  List.map ~f:(fun s ->
    match String.split s ~on:',' with
    | [x; y] -> (Int.of_string x, Int.of_string y)
    | _ -> invalid_arg "bad parse")

let p1ans = List.cartesian_product rtiles rtiles |>
    List.map ~f:(fun ((x0, y0), (x1, y1)) -> (abs @@ x1 - x0 + 1) * (abs @@ y1 - y0 + 1)) |>
    List.max_elt ~compare:Int.compare |>
    Option.value_exn

let () = printf "p1 answer: %d\n" p1ans