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
  let neigh_cells = List.map adjs ~f:(fun l -> at grid l) in
  List.fold neigh_cells ~init:0 ~f:(fun acc mc -> 
  match mc with
  | Some Paper -> acc + 1
  | _ -> acc)

let p1ans = 
  let paper_locs =
    all_cells parsed_grid |>
    List.filter ~f:(fun c -> match at parsed_grid c with
                    | Some Paper -> true
                    | _ -> false) in
  List.count paper_locs ~f:(fun loc -> 
    (adjacent_papers parsed_grid loc) < 4)

let () = printf "p1 accessible papers: %d\n" p1ans

(* let () =
  let h = Array.length parsed_grid in
  let w = Array.length parsed_grid in
  for y = 0 to (h - 1) do
    for x = 0 to (w - 1) do
      let cell = at parsed_grid (x, y) in
      match cell with
      | Some Floor -> print_string "."
      | Some Paper ->
        let adjpaper = adjacent_papers parsed_grid (x, y) in
        (* let accessible = (adjpaper < 4) in *)
        if adjpaper < 10 then
           printf "%d" adjpaper 
        else 
           print_string "!"
      | _ -> ()
    done;
    print_string "\n"
  done *)
