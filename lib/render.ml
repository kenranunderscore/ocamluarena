(* TODO: decoupling rendering from the engine is probably the way to go. that
   means projecting engine state to something renderable and then rendering in a
   separate thread. that way the frame rate would be independent of the ticks
   per second as well. *)

module State = Game.State

let heading renderer (settings : Settings.t) (p : Point.t) h =
  let radius = settings.player_radius in
  let len = radius +. 10. in
  Sdl.set_render_draw_color renderer ~red:10 ~green:250 ~blue:50;
  Sdl.draw_line_in_direction renderer p h len;
  Sdl.draw_line_in_direction renderer p (h +. Float.pi) radius;
  Sdl.draw_line_in_direction renderer p (h +. Math.half_pi) radius;
  Sdl.draw_line_in_direction renderer p (h -. Math.half_pi) radius
;;

let view_angle renderer (settings : Settings.t) (p : Point.t) angle =
  (* TODO: bind SDL_RenderGeometry and use that to fill the area *)
  let left_angle = angle -. (settings.player_angle_of_vision /. 2.) in
  let right_angle = angle +. (settings.player_angle_of_vision /. 2.) in
  Sdl.set_render_draw_color renderer ~red:20 ~green:100 ~blue:100;
  Sdl.draw_line_in_direction renderer p angle 50.;
  Sdl.draw_line_in_direction renderer p left_angle 5000.;
  Sdl.draw_line_in_direction renderer p right_angle 5000.
;;

let player
  renderer
  (settings : Settings.t)
  (meta : Player.meta)
  (player_state : Player_state.t)
  =
  let { Color.red; green; blue } = meta.color in
  let p = player_state.pos in
  Sdl.set_render_draw_color renderer ~red ~green ~blue;
  Sdl.draw_circle renderer p settings.player_radius;
  heading renderer settings player_state.pos player_state.heading;
  view_angle renderer settings player_state.pos player_state.view_direction
;;

let players renderer (game : Game.t) =
  game.state
  |> State.living_players
  |> Players.iter (fun _id { Game.player_state; impl } ->
    player renderer game.settings impl.meta player_state)
;;

let attack renderer (attack : Game.attack_state) =
  Sdl.set_render_draw_color renderer ~red:255 ~green:255 ~blue:255;
  Sdl.draw_circle renderer attack.pos Game.attack_radius
;;

let attacks renderer (attacks : Game.attack_state list) =
  attacks |> List.iter (attack renderer)
;;

let scene renderer (game : Game.t) =
  players renderer game;
  game.state.attacks |> Players.iter (fun _id atts -> attacks renderer atts)
;;
