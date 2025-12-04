open Base

type 'a t = {
  w: int;
  h: int;
  grid: 'a Array.t
}

let at g (x, y) =
  if x < 0 || x >= g.w then None else
  if y < 0 || y >= g.h then None else
  Some g.grid.(y * g.w + x)

let all_cells g =
  let cells = ref [] in
  for y = 0 to (g.h - 1) do
    for x = 0 to (g.w - 1) do
      cells := (x, y) :: !cells
    done
  done;
  List.rev !cells

let adjacents (x, y) =
  [(x - 1, y - 1); (x, y - 1); (x + 1, y - 1);
   (x - 1, y)    ; (* self *)  (x + 1, y);
   (x - 1, y + 1); (x, y + 1); (x + 1, y + 1)]

let of_2d_array ga =
  let h = Array.length ga in
  let w = Array.length ga.(0) in
  let ol = Array.to_list ga in
  { h; w; grid = Array.concat ol }

let copy g =
  { h = g.h; w = g.w; grid = Array.copy g.grid }

let set g (x, y) v =
  Array.set g.grid (y * g.w + x) v