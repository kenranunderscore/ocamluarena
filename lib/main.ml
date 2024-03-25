module type PLAYER = Player.PLAYER

module Player_map = Engine.Player_map
module Game_state = Engine.Game_state

let global_scale = 2.0

let main_loop renderer (initial_game_state : Game_state.t) =
  let e = Sdl.Event.create () in
  let quit = ref false in
  let tick = ref 0 in
  let game_state = ref initial_game_state in
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
  (* TODO: implement add_player s.t. ids and state are distributed
     automatically, with initial position and heading being randomized (seeded),
     and based on the arena + players. Therefore the player insertion order
     should perhaps be randomized as well. *)
  let mk_state_reader (r : Engine.player_state ref) () =
    let { Engine.pos; heading; hp; _ } = !r in
    { Player.pos; heading; hp }
  in
  let state1 =
    ref @@ Engine.make_initial_state { x = 200.; y = 50. } (2. *. Float.pi /. 3.)
  in
  let impl1 = Player.Lua.load "lloyd.lua" (mk_state_reader state1) in
  let state3 =
    ref @@ Engine.make_initial_state { x = 600.; y = 500. } ((2. *. Float.pi) -. 1.)
  in
  let game_state =
    Game_state.initial
    |> Game_state.add_player state1 impl1
    |> Game_state.add_player state3 (module Kai)
  in
  Sdl.with_sdl (fun () ->
    Sdl.with_window_and_renderer
      ~w:(Int.of_float @@ (global_scale *. Engine.arena_width))
      ~h:(Int.of_float @@ (global_scale *. Engine.arena_height))
      "Arena"
      (fun _window renderer -> main_loop renderer game_state))
;;
