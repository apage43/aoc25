open Base
open Stdio

let argv = Sys.get_argv ()
let infilename = argv.(1)

let split_at_blank sl =
  let (blankp, _) = List.findi sl ~f:(fun _ s -> String.equal s "") |> Option.value_exn in
  let (f, r) = List.split_n sl blankp in
  (f, List.drop r 1)

let parse_range s = match String.split_on_chars ~on:['-'] s with
  | lo :: hi :: [] -> (Int.of_string lo, Int.of_string hi)
  | _ -> invalid_arg "oops"

let fresh_ranges, ingredients = 
  let (freshrange_sl, ingredients_sl) = 
    In_channel.read_lines infilename |> split_at_blank in
    (List.map ~f:parse_range freshrange_sl, List.map ~f:Int.of_string ingredients_sl)

let in_any intervals el =
  List.exists intervals ~f:(fun (lo, hi) -> el >= lo && el <= hi)

let num_fresh_p1 = ingredients |> List.count ~f:(in_any fresh_ranges)

let () = printf "p1 num fresh: %d\n" num_fresh_p1