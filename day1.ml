open Base
open Stdio

let argv = Sys.get_argv ()
let infilename = argv.(1)

let lines = In_channel.read_lines infilename

type turn = Left | Right [@@deriving sexp]
type instr = turn * int [@@deriving sexp]

let parse s =
  if String.length s < 2 then invalid_arg "parse" else
  let dir =
    match s.[0] with
    | 'L' -> Left
    | 'R' -> Right
    | _   -> invalid_arg "parse: expected L or R"
  in
  let n = Int.of_string (String.sub s ~pos:1 ~len:(String.length s - 1)) in
  (dir, n)

let parsed: instr list = List.map ~f:parse lines

(*
let sp = sexp_of_list sexp_of_instr parsed
let () = printf "\n%s\n" (Sexp.to_string_hum sp)
*)

let rec dial n =
  if n > 99 then dial (n - 100) else
  if n < 0 then dial (n + 100) else
  n

let p1zeroes =
  let rec loop zeroes pos instrs =
    match instrs with
    | i :: rest ->
      let pos =
        match i with
        | (Left, n) -> dial (pos - n)
        | (Right, n) -> dial (pos + n)
      in
      let zeroes = if pos = 0 then zeroes + 1 else zeroes in
      loop zeroes pos rest
    | [] -> zeroes
  in loop 0 50 parsed

let () = printf "part 1 zeroes: %d\n" p1zeroes

let step_and_count pos delta =
  let dir = if delta >= 0 then 1 else -1 in
  let steps = abs delta in
  let zeroes = ref 0 in
  let pos = ref pos in
  for _ = 1 to steps do
    pos := dial (!pos + dir);
    if !pos = 0 then Int.incr zeroes
  done;
  (!pos, !zeroes)

let p2zeroes =
  let zeroes = ref 0 in
  let pos = ref 50 in
  List.iter parsed ~f:(fun (dir, n) ->
    let delta = match dir with Left -> -n | Right -> n in
    let new_pos, crossed = step_and_count !pos delta in
    pos := new_pos;
    zeroes := !zeroes + crossed
  );
  !zeroes

let () = printf "part 2 zeroes: %d\n" p2zeroes
