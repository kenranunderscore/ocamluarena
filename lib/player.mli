type meta =
  { name : string
  ; color : Color.t
  }

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
end

module type PLAYER = sig
  val meta : meta
  val on_tick : int -> command list
  val on_enemy_seen : string -> Point.t -> command list
  val on_attack_hit : string -> Point.t -> command list
  val on_hit_by : string -> command list
  val on_death : unit -> unit
end

module Lua : sig
  val load : string -> (unit -> player_info) -> (module PLAYER)
end
