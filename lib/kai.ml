(** An example player implementation that doesn't use Lua at all. Something like
    this might come in handy for testing. *)

let meta = { Player.name = "Kai"; color = Color.make ~red:240 ~green:10 ~blue:20 }

let on_tick _tick =
  print_endline "kai moving";
  [ Player.Move 1. ]
;;
