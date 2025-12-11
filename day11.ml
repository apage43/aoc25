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