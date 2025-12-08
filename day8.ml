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
  for i = 0 to len - 1 do
    for j = i + 1 to len - 1 do
      p := (i,j) :: !p
    done
  done; !p

let idxp_euc_dist (i0, i1) =
  euc_dist input.(i0) input.(i1)

let pairs_by_dist =
  let inlen = Array.length input in
  let all_pairs = uniq_pairs inlen in
  List.sort all_pairs ~compare:(fun a b -> Float.compare (idxp_euc_dist a) (idxp_euc_dist b))

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

let p2ans =
  let cl = List.length pairs_by_dist in
  let ncc nedges = 
    let cc = connected_components_edges ~num_vertices:(Array.length input) (List.take pairs_by_dist nedges) in
    List.length cc in
  let rec bs lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if ncc mid > 1 then bs (mid + 1) hi else bs lo mid in
  let breakpoint = bs 0 cl in
  let pair = List.nth_exn pairs_by_dist (breakpoint - 1) in
  let boxes = (fun (a, b) -> input.(a), input.(b)) pair in
  let ((x0, _, _), (x1, _, _)) = boxes in
  x0 * x1

let () = printf "p2 ans: %d\n" p2ans