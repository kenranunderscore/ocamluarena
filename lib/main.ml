module State = Game.State

let global_scale = 2.0

let main_loop renderer get_game =
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
    Render.scene renderer (get_game ());
    Sdl.render_present renderer
  done
;;

let discover_players dir =
  dir
  |> Sys.readdir
  |> Array.to_list
  |> List.map (Filename.concat dir)
  |> List.filter Sys.is_directory
  |> List.filter_map (fun d ->
    match Player.Lua.read_meta d with
    | Some meta -> Some (meta, d)
    | None -> None)
;;

let main () =
  Random.self_init ();
  let settings = Settings.make (Random.bits ()) in
  let players = discover_players settings.player_directory in
  let game_ref = Game.init settings players in
  let (_ : unit Domain.t) = Domain.spawn (fun _ -> Game.run game_ref) in
  Sdl.with_sdl (fun () ->
    Sdl.with_window_and_renderer
      ~w:(Int.of_float @@ (global_scale *. settings.arena_width))
      ~h:(Int.of_float @@ (global_scale *. settings.arena_height))
      "Arena"
      (fun _window renderer -> main_loop renderer (fun () -> !game_ref)))
;;
