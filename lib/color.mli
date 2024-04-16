type t =
  { red : int
  ; green : int
  ; blue : int
  }

val make : red:int -> green:int -> blue:int -> t
val random : unit -> t
