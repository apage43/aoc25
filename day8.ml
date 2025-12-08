open Base
open Stdio

let argv = Sys.get_argv ()
let infilename = argv.(1)

let read_int_triple s =
  match (String.split s ~on:',' |> List.map ~f:Int.of_string) with
  | [x;y;z] -> (x,y,z)
  | _ -> invalid_arg "invalid triple"

let input = In_channel.read_lines infilename |> List.map ~f:read_int_triple |> Array.of_list

let euc_dist (x1,y1,z1) (x2,y2,z2) =
  let dx = x1 - x2
  and dy = y1 - y2
  and dz = z1 - z2 in
  Float.sqrt (Float.of_int (dx*dx + dy*dy + dz*dz))


let uniq_pairs len =
  let p = ref [] in
  for i0 = 0 to len - 1 do
    for i1 = 0 to len - 1 do
      if not @@ Int.equal i0 i1 then
        p := (min i0 i1, max i0 i1) :: !p
      else ()
    done  
  done;
  List.dedup_and_sort !p ~compare:(fun (a0, b0) (a1, b1) -> 
    let s0 = Int.compare a0 a1 in
    if s0 <> 0 then s0 else Int.compare b0 b1)
  

let idxp_euc_dist (i0, i1) =
  euc_dist input.(i0) input.(i1)

let pairs_by_dist =
  let inlen = Array.length input in
  let all_pairs = uniq_pairs inlen in
  List.sort all_pairs ~compare:(Comparable.lift Float.ascending ~f:idxp_euc_dist)

let connected_components (g : int list array) =
  let n = Array.length g in
  let visited = Array.create ~len:n false in
  let rec dfs v acc =
    visited.(v) <- true;
    List.fold g.(v) ~init:(v :: acc) ~f:(fun acc u ->
      if not visited.(u) then dfs u acc else acc)
  in
  let components = ref [] in
  for v = 0 to n - 1 do
    if not visited.(v) then
      components := dfs v [] :: !components
  done;
  !components

let connected_components_edges ~num_vertices edges =
  (* printf "connecting edges: %s\n" @@ Sexp.to_string_hum ([%sexp_of: (int * int) list] edges); *)
  let g = Array.create ~len:num_vertices [] in
  List.iter edges ~f:(fun (u,v) ->
    g.(u) <- v :: g.(u);
    g.(v) <- u :: g.(v));
  connected_components g

let p1ans n_edges n_components =
  let cc = connected_components_edges ~num_vertices:(Array.length input) (List.take pairs_by_dist n_edges) 
  |> List.sort ~compare:(Comparable.lift Int.descending ~f:List.length) in
  (* printf "cc: %s\n" @@ Sexp.to_string_hum ([%sexp_of: int list list] cc); *)
  List.take cc n_components |> List.map ~f:List.length |> List.reduce ~f:( * ) |> Option.value_exn

let () = 
  if String.is_substring infilename ~substring:"example" then
    printf "p1 ans (example): %d\n" @@ p1ans 10 3
  else
    printf "p1 ans (actual): %d\n" @@ p1ans 1000 3