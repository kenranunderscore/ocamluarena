module Movement = struct
  type t =
    { distance : float
    ; direction : Player.movement_direction
    }

  let default = { distance = 0.0; direction = Forward }
end

type t =
  { movement : Movement.t
  ; turn_angle : float
  ; view_angle : float
  ; attack : float option
  }

let default =
  { movement = Movement.default; turn_angle = 0.0; view_angle = 0.0; attack = None }
;;

let set_movement intent movement = { intent with movement }
