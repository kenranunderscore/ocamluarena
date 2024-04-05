module type PLAYER = Player.PLAYER

module Player_map = Game.Player_map
module State = Game.State

let global_scale = 2.0

let main_loop renderer get_state =
  let e = Sdl.Event.create () in
  let quit = ref false in
  Sdl.scale renderer global_scale;
  while not !quit do
    while Sdl.poll_event e && not !quit do
      match Sdl.Event.(enum (get e typ)) with
      | `Quit -> quit := true
      | `Key_up ->
        (match Sdl.Event.(get e keyboard_scancode) |> Sdl.Scancode.enum with
         | `Escape -> quit := true
         | _ -> ())
      | _ -> ()
    done;
    Sdl.set_render_draw_color renderer ~red:20 ~green:20 ~blue:20;
    Sdl.render_clear renderer;
    Render.scene renderer (get_state ());
    Sdl.render_present renderer
  done
;;

let main () =
  Random.self_init ();
  let rounds = 3 in
  let state = Game.State.init [ "kai.lua"; "lloyd.lua" ] rounds in
  let _ = Domain.spawn (fun _ -> Game.run state rounds) in
  Sdl.with_sdl (fun () ->
    Sdl.with_window_and_renderer
      ~w:(Int.of_float @@ (global_scale *. Game.arena_width))
      ~h:(Int.of_float @@ (global_scale *. Game.arena_height))
      "Arena"
      (fun _window renderer -> main_loop renderer (fun () -> !state)))
;;
