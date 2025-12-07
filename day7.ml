open Base
open Stdio

let argv = Sys.get_argv ()
let infilename = argv.(1)

let (source, splitters) = 
  let lines = In_channel.read_lines infilename in
  let bp0 = List.hd_exn lines |> (Fn.flip String.index_exn) 'S' in
  let splitters = List.filter lines ~f:((Fn.flip String.mem) '^')
  |> List.map ~f:(fun s ->
    String.to_list s
    |> List.filter_mapi ~f:(fun i c ->
      if Char.equal c '^' then Some i else None)) in
  (bp0, splitters)

let split_beams_1 splitcounter in_beams splitters =
  let out_beams = ref @@ Set.of_list (module Int) in_beams in
  let () = List.iter splitters ~f:(fun si ->
    if List.mem in_beams si ~equal:Int.equal then 
      (splitcounter := !splitcounter + 1;
       out_beams := Set.remove !out_beams si 
        |> (Fn.flip Set.add) (si - 1)
        |> (Fn.flip Set.add) (si + 1))
    else
      ()) in
  Set.to_list !out_beams

let p1ans =
  let splitcounter = ref 0 in
  let _ = List.fold ~init:[source] ~f:(split_beams_1 splitcounter) splitters in
  !splitcounter

let () = printf "p1 num splits: %d\n" p1ans