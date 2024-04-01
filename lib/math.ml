let sign x = if Float.sign_bit x then -1. else 1.
let is_between x a b = a <= x && x <= b

(* TODO: inline? *)
let circles_intersect (p1 : Point.t) r1 (p2 : Point.t) r2 = Point.dist p1 p2 <= r1 +. r2

let angle_between (p : Point.t) (q : Point.t) =
  let dy = q.y -. p.y in
  let dx = q.x -. p.x in
  Float.atan2 dy dx +. (Float.pi /. 2.)
;;

let normalize_angle angle =
  (* TODO: use this as an example for checking out testing in OCaml *)
  (* FIXME: do this purely and compare performance *)
  (* might also be wrong still *)
  let res = ref angle in
  while !res > 2. *. Float.pi do
    res := !res -. (2. *. Float.pi)
  done;
  while !res < 0. do
    res := !res +. (2. *. Float.pi)
  done;
  !res
;;
