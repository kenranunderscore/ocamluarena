type t =
  { arena_width : float
  ; arena_height : float
  ; player_radius : float
  ; player_angle_of_vision : float
  ; max_turn_rate : float
  ; max_view_turn_rate : float
  ; rounds : int
  ; rng_seed : int
  ; player_directory : string
  }
[@@deriving show]

let make rng_seed =
  { arena_width = 1000.
  ; arena_height = 800.
  ; player_radius = 25.
  ; player_angle_of_vision = 0.9 *. Math.half_pi
  ; max_turn_rate = Math.to_radians 5.
  ; max_view_turn_rate = Math.to_radians 10.
  ; rounds = 3
  ; rng_seed
  ; player_directory = "./players"
  }
;;
