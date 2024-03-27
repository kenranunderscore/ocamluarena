type t =
  { x : float
  ; y : float
  }

let make ~x ~y = { x; y }
let dist_sqr p1 p2 = Float.pow (p1.x -. p2.x) 2. +. Float.pow (p1.y -. p2.y) 2.
let dist p1 p2 = sqrt (dist_sqr p1 p2)
