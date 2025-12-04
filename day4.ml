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
  List.to_array |>
  Grid.of_2d_array

let adjacent_papers grid loc =
  let adjs = Grid.adjacents loc in
  let neigh_cells = List.map adjs ~f:(Grid.at grid) in
  List.count neigh_cells ~f:(
    fun mc -> match mc with
    | Some Paper -> true
    | _ -> false)

let accessible_papers grid = 
  let paper_locs =
    Grid.all_cells grid |>
    List.filter ~f:(fun c -> match Grid.at grid c with
                    | Some Paper -> true
                    | _ -> false) in
  List.filter paper_locs ~f:(fun loc -> 
    (adjacent_papers grid loc) < 4)

let p1ans = accessible_papers parsed_grid |> List.length

let () = printf "p1 accessible papers: %d\n" p1ans

let p2ans =
  let work_grid = Grid.copy parsed_grid in
  let rec loop grid removed =
    let round_removed = ref 0 in
    let removable = accessible_papers grid in
    let () = List.iter removable ~f:(fun loc ->
      Grid.set grid loc Floor;
      round_removed := !round_removed + 1
    ) in match !round_removed with
    | 0 -> removed
    | rr -> loop grid removed + rr
    in loop work_grid 0

let () = printf "p2 removable papers: %d\n" p2ans