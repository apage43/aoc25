open Base
open Stdio

let argv = Sys.get_argv ()
let infilename = argv.(1)

exception BadParse of string

let parse_range s = match String.split_on_chars ~on:['-'] s with
  | lo :: hi :: [] -> (Int.of_string lo, Int.of_string hi)
  | _ -> raise @@ BadParse "oops"

let ranges =
  In_channel.read_all infilename |>
  String.strip |>
  String.split_on_chars ~on:[','] |>
  List.map ~f:parse_range

let is_invalid_p1 i =
  let istr = Int.to_string i in
  let slen = String.length istr in
  if slen % 2 = 1 then false else
    let half = slen / 2 in
    let front = String.prefix istr half in
    let back = String.suffix istr half in
    String.equal front back

let range_invalids (lo, hi) =
  List.init (hi - lo + 1) ~f:((+) lo) |>
  List.filter ~f:is_invalid_p1

let sum_invalids = ranges |>
  List.map ~f:range_invalids |>
  List.concat |>
  List.fold ~init:0 ~f:(+)

let () = printf "p1 invalids: %d\n" sum_invalids
(*
let () = List.iter ~f:(fun (lo, hi) -> printf "%s-%s," lo hi) ranges
let () = printf "%s\n" ranges
*)
