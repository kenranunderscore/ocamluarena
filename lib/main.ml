module type PLAYER = Player.PLAYER

module Player_map = Engine.Player_map
module Game_state = Engine.Game_state
module Game = Engine.Game

let global_scale = 2.0

let main_loop renderer =
  let e = Sdl.Event.create () in
  let quit = ref false in
  let tick = ref 0 in
  let game_state = ref (Game.start_new [ "lloyd.lua" ]) in
  Sdl.scale renderer global_scale;
  while not !quit do
    tick := !tick + 1;
    while Sdl.poll_event e && not !quit do
      match Sdl.Event.(enum (get e typ)) with
      | `Quit -> quit := true
      | `Key_up ->
        (match Sdl.Event.(get e keyboard_scancode) |> Sdl.Scancode.enum with
         | `Escape -> quit := true
         | _ -> ())
      | _ -> ()
    done;
    game_state := Engine.step !game_state !tick;
    Sdl.set_render_draw_color renderer ~red:20 ~green:20 ~blue:20;
    Sdl.render_clear renderer;
    Render.scene renderer !game_state;
    Sdl.render_present renderer;
    Thread.delay 0.01
  done
;;

let main () =
  Sdl.with_sdl (fun () ->
    Sdl.with_window_and_renderer
      ~w:(Int.of_float @@ (global_scale *. Engine.arena_width))
      ~h:(Int.of_float @@ (global_scale *. Engine.arena_height))
      "Arena"
      (fun _window renderer -> main_loop renderer))
;;
