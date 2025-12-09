open Base
open Stdio

let argv = Sys.get_argv ()
let infilename = argv.(1)

let rtiles = In_channel.read_lines infilename |> 
  List.map ~f:(fun s ->
    match String.split s ~on:',' with
    | [x; y] -> (Int.of_string x, Int.of_string y)
    | _ -> invalid_arg "bad parse")

let p1ans = List.cartesian_product rtiles rtiles |>
    List.map ~f:(fun ((x0, y0), (x1, y1)) -> (abs (x1 - x0) + 1) * (abs (y1 - y0) + 1)) |>
    List.max_elt ~compare:Int.compare |>
    Option.value_exn

let () = printf "p1 answer: %d\n" p1ans

let consec_pairs lst = List.zip_exn (List.drop_last_exn lst) (List.tl_exn lst)
  
let outline = consec_pairs (List.last_exn rtiles :: rtiles)

let point_in_polygon segs (px, py) =
  let on_edge = List.exists segs ~f:(fun ((x0, y0), (x1, y1)) ->
    if x0 = x1 then
      x0 = px && Int.min y0 y1 <= py && py <= Int.max y0 y1
    else
      y0 = py && Int.min x0 x1 <= px && px <= Int.max x0 x1
  ) in
  if on_edge then true
  else
    let count = List.count segs ~f:(fun ((x0, y0), (x1, y1)) ->
      if y0 <> y1 then false
      else
        y0 > py &&
        let x_lo = Int.min x0 x1 in
        let x_hi = Int.max x0 x1 in
        x_lo <= px && px < x_hi
    ) in
    count % 2 = 1

let rect_in_polygon segs (x_min, y_min) (x_max, y_max) =
  point_in_polygon segs (x_min, y_min) &&
  point_in_polygon segs (x_min, y_max) &&
  point_in_polygon segs (x_max, y_min) &&
  point_in_polygon segs (x_max, y_max) &&
  not (List.exists rtiles ~f:(fun (x, y) ->
    x_min < x && x < x_max && y_min < y && y < y_max)) &&
  not (List.exists segs ~f:(fun ((x0, y0), (x1, y1)) ->
    if x0 = x1 then
      x_min < x0 && x0 < x_max &&
      Int.min y0 y1 < y_max && Int.max y0 y1 > y_min
    else
      y_min < y0 && y0 < y_max &&
      Int.min x0 x1 < x_max && Int.max x0 x1 > x_min))

let valid_rects = List.cartesian_product rtiles rtiles |>
    List.filter ~f:(fun ((x0, y0), (x1, y1)) -> 
      rect_in_polygon outline (Int.min x0 x1, Int.min y0 y1) (Int.max x0 x1, Int.max y0 y1))

let p2ans = valid_rects |>
    List.map ~f:(fun ((x0, y0), (x1, y1)) -> 
      let area = (abs (x1 - x0) + 1) * (abs (y1 - y0) + 1) in
      area) |>
    List.max_elt ~compare:Int.compare |>
    Option.value_exn

let () = printf "p2 answer: %d\n" p2ans