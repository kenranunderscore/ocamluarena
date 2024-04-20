type t =
  { name : string
  ; color : Color.t
  ; version : string
  ; entrypoint : string
  }

val compare : t -> t -> int

type movement_direction =
  | Forward
  | Backward
  | Left
  | Right
[@@deriving show]

module Command : sig
  type t =
    | Move of movement_direction * float
    | Attack
    | Turn of float
    | Turn_head of float
    | Turn_arms of float
  [@@deriving show]

  val index : t -> int
  val compare : t -> t -> int
end

type info =
  { hp : int
  ; pos : Point.t
  ; heading : float
  ; head_direction : float
  ; arms_direction : float
  ; turn_remaining : float
  ; head_turn_remaining : float
  ; arms_turn_remaining : float
  }

module Id : sig
  type t

  val make : int -> t
  val compare : t -> t -> int
  val show : t -> string
end

type impl =
  { on_round_started : int -> Command.t list
  ; on_tick : int -> Command.t list
  ; on_enemy_seen : string -> Point.t -> Command.t list
  ; on_enemy_attack : string -> Command.t list
  ; on_attack_hit : string -> Point.t -> Command.t list
  ; on_hit_by : string -> Command.t list
  ; on_death : unit -> unit
  ; on_round_over : string option -> unit
  ; on_round_won : unit -> unit
  }

module Lua : sig
  val load_implementation : string -> string -> (unit -> info) -> impl
  val read_meta : string -> t option
end
