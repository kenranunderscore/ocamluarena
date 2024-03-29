let angle_between (p : Point.t) (q : Point.t) =
  let dy = q.y -. p.y in
  let dx = q.x -. p.x in
  Float.atan2 dy dx +. (Float.pi /. 2.)
;;

let is_between x a b = a <= x && x <= b

let normalize_angle angle =
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
