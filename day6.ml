open Base
open Stdio

let argv = Sys.get_argv ()
let infilename = argv.(1)

let wsplit s = 
  String.split ~on:' ' s |>
  List.filter ~f:(Fn.non String.is_empty)

let intline s = wsplit s |> List.map ~f:Int.of_string

let parsed =
  let lines = In_channel.read_lines infilename |> List.rev in
  match lines with
  | [] -> invalid_arg "bad input"
  | ops_s :: operands_s ->
  (wsplit ops_s |> List.map ~f:(function 
    | "+" -> ( + )
    | "*" -> ( * )
    | _ -> invalid_arg "bad operator"
  ), List.map ~f:intline operands_s |> List.transpose |> Option.value_exn)

let p1ans = 
  let operators, operands = parsed in
  List.map2_exn operators operands ~f:(fun op operands -> 
    List.reduce ~f:op operands |> Option.value_exn
  ) |> List.reduce ~f:(+) |> Option.value_exn

let () = printf "part1 total: %d\n" p1ans