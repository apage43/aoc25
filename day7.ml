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

let split_1_beam inb splitters =
  if List.mem splitters inb ~equal:Int.equal then
    [inb - 1; inb + 1]
  else
    [inb]

(* let timelines tls splitters =
  let tlc = ref 1 in
  let () = let rec loop nexts sprest =
    (match nexts with
     | _ :: [] | [] -> ()
     | _ :: _ -> tlc := !tlc + 1);
    match sprest with
    | [] -> ()
    | sp :: sr -> List.iter nexts ~f:(fun nb -> loop (split_1_beam nb sp) sr)
  in loop tls splitters in
  !tlc *)

let timelines tls splitters =
  let memo = Hashtbl.create (module struct
    type t = int * int [@@deriving compare, hash, sexp]
  end) in
  
  let rec count_from beam_pos splitter_idx =
    if splitter_idx >= List.length splitters then
      1
    else
      match Hashtbl.find memo (beam_pos, splitter_idx) with
      | Some result -> result
      | None ->
          let sp = List.nth_exn splitters splitter_idx in
          let next_beams = split_1_beam beam_pos sp in
          let result = List.sum (module Int) next_beams 
            ~f:(fun nb -> count_from nb (splitter_idx + 1)) in
          Hashtbl.set memo ~key:(beam_pos, splitter_idx) ~data:result;
          result
  in
  List.sum (module Int) tls ~f:(fun b -> count_from b 0)

let p2ans = timelines [source] splitters

let () = printf "p2 num timelines: %d\n" p2ans