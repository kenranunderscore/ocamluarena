type t =
  { pos : Point.t
  ; heading : float
  ; view_direction : float
  ; intent : Intent.t
  ; attack_cooldown : int
  ; hp : int
  }

let is_alive s = s.hp > 0
let is_dead s = not (is_alive s)
