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

let range_invalids icheck (lo, hi) =
  List.init (hi - lo + 1) ~f:((+) lo) |>
  List.filter ~f:icheck

let sum_invalids rcheck = ranges |>
  List.map ~f:rcheck |>
  List.concat |>
  List.fold ~init:0 ~f:(+)

let () = printf "p1 invalids: %d\n" @@ sum_invalids @@ range_invalids is_invalid_p1

let chunk s ~size =
  let len = String.length s in
  List.init ((len + size - 1) / size) ~f:(fun i ->
    let start = i * size in
    let stop = Int.min (start + size) len in
    String.sub s ~pos:start ~len:(stop - start))

let is_invalid_p2 i =
  let istr = Int.to_string i in
  let slen = String.length istr in
  let checkcyclen cyclen = 
  if not (slen % cyclen = 0) then false else
    let chunks = chunk istr ~size:cyclen in
    match chunks with 
    | [] -> false
    | chunk0 :: _ -> List.for_all chunks ~f:(String.equal chunk0)
  in 
  List.init (slen - 1) ~f:((+) 1) |>
  List.exists ~f:checkcyclen

let () = printf "p2 invalids: %d\n" @@ sum_invalids @@ range_invalids is_invalid_p2

(*
let () = List.iter ~f:(fun (lo, hi) -> printf "%s-%s," lo hi) ranges
let () = printf "%s\n" ranges
*)
