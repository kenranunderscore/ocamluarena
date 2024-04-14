type t =
  { name : string
  ; color : Color.t
  ; version : string
  }

val compare : t -> t -> int

type movement_direction =
  | Forward
  | Backward
  | Left
  | Right
[@@deriving show]

type command =
  | Move of movement_direction * float
  | Turn_right of float
  | Attack of float
  | Look_right of float
[@@deriving show]

val command_index : command -> int

type player_info =
  { hp : int
  ; pos : Point.t
  ; heading : float
  ; view_direction : float
  }

module Id : sig
  type t

  val make : int -> t
  val compare : t -> t -> int
  val show : t -> string
end

type impl =
  { on_round_started : int -> command list
  ; on_tick : int -> command list
  ; on_enemy_seen : string -> Point.t -> command list
  ; on_attack_hit : string -> Point.t -> command list
  ; on_hit_by : string -> command list
  ; on_death : unit -> unit
  ; on_round_over : string option -> unit
  ; on_round_won : unit -> unit
  }

module Lua : sig
  val load_implementation : string -> string -> (unit -> player_info) -> impl
  val read_meta : string -> t option
end
