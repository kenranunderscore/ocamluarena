(* TODO: decoupling rendering from the engine is probably the way to go. that
   means projecting engine state to something renderable and then rendering in a
   separate thread. that way the frame rate would be independent of the ticks
   per second as well. *)

module Game_state = Engine.Game_state
module Player_map = Engine.Player_map

module type PLAYER = Player.PLAYER

let heading renderer (p : Point.t) h =
  let len = Engine.player_radius +. 10. in
  let x = p.x +. (len *. Float.sin h) in
  let y = p.y -. (len *. Float.cos h) in
  let dest = Point.make ~x ~y in
  Sdl.set_render_draw_color renderer ~red:10 ~green:250 ~blue:50;
  Sdl.draw_line renderer p dest
;;

let player renderer (meta : Player.meta) (player_state : Engine.player_state) =
  let { Color.red; green; blue } = meta.color in
  let p = player_state.pos in
  Sdl.set_render_draw_color renderer ~red ~green ~blue;
  Sdl.draw_circle renderer p Engine.player_radius;
  heading renderer player_state.pos player_state.heading
;;

let players renderer (game_state : Game_state.t) =
  game_state.living_players
  |> Player_map.iter (fun _id { Engine.state; impl } ->
    let module M = (val impl : PLAYER) in
    player renderer M.meta !state)
;;

let attack renderer (attack : Engine.attack_state) =
  Sdl.set_render_draw_color renderer ~red:255 ~green:255 ~blue:255;
  Sdl.draw_circle renderer attack.pos Engine.attack_radius
;;

let attacks renderer (attacks : Engine.attack_state list) =
  attacks |> List.iter (attack renderer)
;;

let scene renderer (game_state : Game_state.t) =
  players renderer game_state;
  game_state.attacks |> Player_map.iter (fun _id atts -> attacks renderer atts)
;;
