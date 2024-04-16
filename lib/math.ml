let two_pi = 2. *. Float.pi
let half_pi = Float.pi /. 2.
let sign x = if Float.sign_bit x then -1. else 1.
let is_between x a b = a <= x && x <= b
let to_radians deg = deg *. Float.pi /. 180.
let from_radians rad = rad /. Float.pi *. 180.

(* TODO: inline? *)
let circles_intersect (p1 : Point.t) r1 (p2 : Point.t) r2 = Point.dist p1 p2 <= r1 +. r2

let angle_between (p : Point.t) (q : Point.t) =
  let dy = q.y -. p.y in
  let dx = q.x -. p.x in
  Float.atan2 dy dx +. (Float.pi /. 2.)
;;

let rec normalize_absolute_angle = function
  | angle when angle >= two_pi -> normalize_absolute_angle (angle -. two_pi)
  | angle when angle < 0. -> normalize_absolute_angle (angle +. two_pi)
  | angle -> angle
;;

let normalize_relative_angle = function
  | angle when angle >= -.Float.pi && angle < Float.pi -> angle
  | angle ->
    let abs = normalize_absolute_angle angle in
    if abs >= Float.pi then abs -. two_pi else abs
;;

let clamp x left right = Float.min (Float.max left x) right
