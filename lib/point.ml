type t =
  { x : float
  ; y : float
  }

let dist p1 p2 = sqrt (Float.pow (p1.x -. p2.x) 2. +. Float.pow (p1.y -. p2.y) 2.)
