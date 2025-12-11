open Base
open Stdio

let argv = Sys.get_argv ()
let infilename = argv.(1)

let parse file = In_channel.read_lines file |>
  List.map ~f:(fun line ->
    match String.split ~on:':' line with
    | [dev; outs_s] -> (dev, String.split ~on:' ' outs_s |> List.tl_exn)
    | _ -> invalid_arg "bad parse")
  |> Hashtbl.of_alist_exn (module String)

let input = parse infilename

let paths_from_to tbl from dest =
  let completed = ref [] in
  let rec search path =
      let cur = List.hd_exn path in
      let outs = Hashtbl.find tbl cur
      |> Option.value ~default:[]
      |> List.filter ~f:(Fn.compose not (List.mem path ~equal:String.equal)) in
      List.iter outs ~f:(fun out ->
        if String.equal out dest then
          completed := (out :: path) :: !completed
        else
          search @@ out :: path) in 
  let () = search [from] in
  !completed

let () = printf "p1 answer: %d\n" (paths_from_to input "you" "out" |> List.length)

let count_paths_from_to tbl from dest =
  let memo = Hashtbl.create (module String) in
  let rec count node visited =
    if String.equal node dest then 1
    else if Set.mem visited node then 0
    else
      match Hashtbl.find memo node with
      | Some c -> c
      | None ->
          let visited' = Set.add visited node in
          let outs = Hashtbl.find tbl node |> Option.value ~default:[] in
          let total = List.fold outs ~init:0 ~f:(fun acc out ->
            acc + count out visited') in
          Hashtbl.set memo ~key:node ~data:total;
          total
  in
  count from (Set.empty (module String))

let count_paths_through tbl from node1 node2 dest =
  let c1 = count_paths_from_to tbl from node1 in
  let c2 = count_paths_from_to tbl node1 node2 in
  let c3 = count_paths_from_to tbl node2 dest in
  let c1' = count_paths_from_to tbl from node2 in
  let c2' = count_paths_from_to tbl node2 node1 in
  let c3' = count_paths_from_to tbl node1 dest in
  (c1 * c2 * c3) + (c1' * c2' * c3')


let () = 
  printf "p2 answer: %d\n" (count_paths_through input "svr" "fft" "dac" "out")