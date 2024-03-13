type meta =
  { name : string
  ; color : Color.t
  }

type command =
  | Move of float
  | Turn_right of float
[@@deriving show]

type player_info =
  { hp : int
  ; pos : Point.t
  ; heading : float
  }

module Id : sig
  type t

  val make : int -> t
  val compare : t -> t -> int
end

module type PLAYER = sig
  val meta : meta
  val on_tick : int -> command list
end

module Lua : sig
  val load : string -> (unit -> player_info) -> (module PLAYER)
end
