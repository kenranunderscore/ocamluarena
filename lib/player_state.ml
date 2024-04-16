type t =
  { pos : Point.t
  ; heading : float
  ; view_direction : float
  ; attack_direction : float
  ; intent : Intent.t
  ; attack_cooldown : int
  ; hp : int
  }

let is_alive s = s.hp > 0
let is_dead s = not (is_alive s)

let resulting_view_direction s =
  Math.normalize_absolute_angle (s.heading +. s.view_direction)
;;

let resulting_attack_direction s =
  Math.normalize_absolute_angle (s.heading +. s.attack_direction)
;;
