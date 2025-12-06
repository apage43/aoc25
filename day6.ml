open Base
open Stdio

let argv = Sys.get_argv ()
let infilename = argv.(1)

let wsplit s = 
  String.split ~on:' ' s |>
  List.filter ~f:(Fn.non String.is_empty)

let intline s = wsplit s |> List.map ~f:Int.of_string

let char_to_op = function 
    | '+' -> ( + )
    | '*' -> ( * )
    | _ -> invalid_arg "bad operator"

let p1parsed =
  let lines = In_channel.read_lines infilename |> List.rev in
  match lines with
  | [] -> invalid_arg "bad input"
  | ops_s :: operands_s ->
  (wsplit ops_s |> List.map ~f:(fun s -> char_to_op @@ String.get s 0), 
   List.map ~f:intline operands_s |> List.transpose |> Option.value_exn)

let p1ans = 
  let operators, operands = p1parsed in
  List.map2_exn operators operands ~f:(fun op operands -> 
    List.reduce ~f:op operands |> Option.value_exn
  ) |> List.reduce ~f:(+) |> Option.value_exn

let () = printf "part1 total: %d\n" p1ans


let xcol nl col =
  match List.map nl ~f:(fun s -> String.get s col) with
  | chars -> String.of_list chars |> String.strip |> Int.of_string_opt
  | exception _ -> None

let operands nl oidx =
  let rec loop acc i =
    match xcol nl i with
    | Some n -> loop (n :: acc) (i + 1)
    | None -> acc
  in loop [] oidx

let p2ans =
  let lines = In_channel.read_lines infilename in
  let opline = List.last_exn lines in
  let nlines = List.drop_last_exn lines in
  let (opidx, ops) = List.filter_mapi (String.to_list opline)
    ~f:(fun i c -> if Char.equal ' ' c then None else Some (i, char_to_op c)) |> List.unzip in
  let operand_lists = List.map opidx ~f:(fun i -> operands nlines i) in
  List.map2_exn ops operand_lists ~f:(fun op operands -> List.reduce ~f:op operands |> Option.value_exn) |>
  List.reduce ~f:(+) |> Option.value_exn

let () = printf "part2 total: %d\n" p2ans