open Base
open Stdio

let argv = Sys.get_argv ()
let infilename = argv.(1)

type cell = Floor | Paper

let parse_str s =
  String.strip s |>
  String.to_list |>
  List.map ~f:(fun ch -> match ch with
  | '.' -> Floor
  | '@' -> Paper
  | _ -> invalid_arg "parse: expect . or @") |>
  List.to_array

let parsed_grid = In_channel.read_lines infilename |>
  List.map ~f:parse_str |>
  List.to_array

let at grid (x, y) =
  let h = Array.length grid in
  let w = Array.length grid.(0) in
  if x < 0 || x >= w then None else
  if y < 0 || y >= h then None else
  Some grid.(y).(x)
  
let all_cells grid =
  let h = Array.length grid in
  let w = Array.length grid.(0) in
  let cells = ref [] in
  for y = 0 to (h - 1) do
    for x = 0 to (w - 1) do
      cells := (x, y) :: !cells
    done
  done;
  List.rev !cells

let adjacents (x, y) =
  [(x - 1, y - 1); (x, y - 1); (x + 1, y - 1);
   (x - 1, y)    ; (* self *)  (x + 1, y);
   (x - 1, y + 1); (x, y + 1); (x + 1, y + 1)]

let adjacent_papers grid loc =
  let adjs = adjacents loc in
  let neigh_cells = List.map adjs ~f:(at grid) in
  List.fold neigh_cells ~init:0 ~f:(fun acc mc -> 
  match mc with
  | Some Paper -> acc + 1
  | _ -> acc)

let accessible_papers grid = 
  let paper_locs =
    all_cells grid |>
    List.filter ~f:(fun c -> match at grid c with
                    | Some Paper -> true
                    | _ -> false) in
  List.filter paper_locs ~f:(fun loc -> 
    (adjacent_papers grid loc) < 4)

let p1ans = accessible_papers parsed_grid |> List.length

let () = printf "p1 accessible papers: %d\n" p1ans

let gridcopy grid =
  Array.to_list grid |> 
  List.map ~f:Array.copy |>
  List.to_array

let p2ans =
  let work_grid = gridcopy parsed_grid in
  let rec loop grid removed =
    let round_removed = ref 0 in
    let removable = accessible_papers grid in
    let () = List.iter removable ~f:(fun (x, y) ->
      Array.set grid.(y) x Floor;
      round_removed := !round_removed + 1
    ) in match !round_removed with
    | 0 -> removed
    | rr -> loop grid removed + rr
    in loop work_grid 0

let () = printf "p2 removable papers: %d\n" p2ans