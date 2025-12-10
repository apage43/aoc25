open Base
open Stdio

let argv = Sys.get_argv ()
let infilename = argv.(1)

type machine = {
  lights: bool list;
  buttons: int list list;
  joltages: int list;
} [@@deriving sexp]

let chop_ends s = String.sub s ~pos:1 ~len:((String.length s) - 2)

let parse_machine mline =
  let chunks = String.split ~on:' ' mline in
  let lights = List.hd_exn chunks
    |> chop_ends
    |> String.to_list
    |> List.map ~f:(function
      | '.' -> false
      | '#' -> true
      | _ -> invalid_arg "bad light") in
  let buttons = List.drop_last_exn chunks
  |> List.tl_exn
  |> List.map ~f:(fun s -> chop_ends s |> String.split ~on:',' |> List.map ~f:Int.of_string) in
  let joltages = List.last_exn chunks
  |> chop_ends
  |> String.split ~on:','
  |> List.map ~f:Int.of_string
  in
    { lights; buttons; joltages }
let machines = In_channel.read_lines infilename |> List.map ~f:parse_machine

(* let () = printf "machines:\n%s\n" @@ Sexp.to_string_hum ([%sexp_of: machine list] machines) *)

let push_btn light_state button =
  let lsa = Array.of_list light_state in
  let () = List.iter button ~f:(fun idx -> Array.set lsa idx @@ not lsa.(idx)) in
  List.of_array lsa


let light_search m =
  let module BoolList = struct
    module T = struct
      type t = bool list [@@deriving sexp, compare]
    end
    include T
    include Comparator.Make(T)
  end in
  let empty_state = List.map m.lights ~f:(fun _ -> false) in
  let goal_state = m.lights in
  let rec bfs queue visited =
    match Queue.dequeue queue with
    | None -> failwith "No solution found"
    | Some (press_chain, light_state) ->
        if List.equal Bool.equal light_state goal_state then
          List.length press_chain
        else if Set.mem visited light_state then
          bfs queue visited
        else
          let visited' = Set.add visited light_state in
          List.iteri m.buttons ~f:(fun btn_idx button ->
            let new_state = push_btn light_state button in
            Queue.enqueue queue (btn_idx :: press_chain, new_state)
          );
          bfs queue visited' in
  let queue = Queue.create () in
  Queue.enqueue queue ([], empty_state);
  bfs queue (Set.empty (module BoolList))

let p1_ans =
  let presscounts = List.map machines ~f:light_search in
  List.reduce_exn presscounts ~f:(+)

let () = printf "p1 answer: %d\n" p1_ans