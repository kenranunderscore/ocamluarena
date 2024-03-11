let failwithf f = Printf.ksprintf failwith f
let arena_width = 1000
let arena_height = 800
let player_diameter = 50.

module Player_map = Map.Make (Player.Id)

type intent =
  { distance : float
  ; angle : float
  }

let default_intent = { distance = 0.0; angle = 0.0 }

type player_state =
  { pos : Point.t
  ; intent : intent
  }

let make_state ~pos ~intent = { pos; intent }
let make_initial_state pos = { pos; intent = default_intent }

type player_data =
  { state : player_state ref
  ; impl : (module Player.PLAYER)
  }

module Game_state = struct
  type t = { players : player_data Player_map.t }

  let initial = { players = Player_map.empty }

  let add_player state impl game_state =
    let data = { state; impl } in
    let new_id = 1 + (Player_map.bindings game_state.players |> List.length) in
    { players = Player_map.add (Player.Id.make new_id) data game_state.players }
  ;;
end

let draw_player renderer (meta : Player.meta) (player_state : player_state) =
  let x = player_state.pos.x -. (player_diameter /. 2.) |> Int.of_float in
  let y = player_state.pos.y -. (player_diameter /. 2.) |> Int.of_float in
  let rect = Sdl.Rect.create ~x ~y ~w:50 ~h:50 in
  let color = meta.color in
  Sdl.set_render_draw_color renderer ~r:color.red ~g:color.green ~b:color.blue;
  Sdl.render_fill_rect renderer rect
;;

(* TODO: implement: take the 'latest' (according to event order?) command of
   each type *)
let reduce_commands commands = commands

type movement_change =
  { remaining_intent : intent
  ; target_position : Point.t
  }

let calculate_new_pos (old_pos : Point.t) intent =
  if Float.abs intent.distance > 0.0
  then (
    (* TODO: fix direction *)
    let delta = if Float.sign_bit intent.distance then -1.0 else 1.0 in
    let remaining_intent = { intent with distance = intent.distance -. delta } in
    let target_position = { old_pos with x = old_pos.x +. delta } in
    Some { remaining_intent; target_position })
  else None
;;

let determine_intent old_intent cmds =
  let apply_cmd intent = function
    | Player.Move dist -> { intent with distance = dist }
    | _ -> intent (* TODO: direction + angle *)
  in
  List.fold_left apply_cmd old_intent cmds
;;

let players_collide (pos1 : Point.t) pos2 = Point.dist pos1 pos2 <= player_diameter

(* TODO: only pass "obstacles" instead of whole state *)
let is_valid_position
  (player_id : Player.Id.t)
  ({ x; y } as p : Point.t)
  (game_state : Game_state.t)
  =
  let r = player_diameter /. 2. in
  let stays_inside_arena =
    x -. r >= 0.
    && x +. r <= Float.of_int arena_width
    && y -. r >= 0.
    && y +. r <= Float.of_int arena_height
  in
  let would_hit_other_player =
    game_state.players
    |> Player_map.exists (fun id { state; _ } ->
      player_id != id && players_collide p !state.pos)
  in
  stays_inside_arena && not would_hit_other_player
;;

let find_colliding_players positions =
  let rec go (_player, movement_change) to_check acc =
    (* TODO: find out who collided with whom -> event *)
    let colliding =
      Player_map.filter
        (fun _p m -> players_collide movement_change.target_position m.target_position)
        to_check
    in
    let new_acc = List.append acc (Player_map.bindings colliding) in
    let remaining =
      (* TODO: map difference utility *)
      Player_map.merge
        (fun _player l r ->
          match l, r with
          | Some x, None -> Some x
          | _ -> None)
        to_check
        colliding
    in
    if Player_map.is_empty remaining
    then new_acc
    else (
      let next = Player_map.choose remaining in
      go next (Player_map.remove (fst next) remaining) new_acc)
  in
  match Player_map.choose_opt positions with
  | Some first ->
    let others = Player_map.remove (fst first) positions in
    go first others []
  | None -> []
;;

let run_tick game_state tick =
  let moving_players =
    game_state.players
    |> Player_map.filter_map (fun _id { state; impl } ->
      let module M = (val impl : Player.PLAYER) in
      Printf.printf "  asking player: %s\n%!" M.meta.name;
      let tick_commands = M.on_tick tick in
      let commands = reduce_commands tick_commands in
      let ps = !state in
      let new_intent = determine_intent ps.intent commands in
      state := { ps with intent = new_intent };
      let desired_movement = calculate_new_pos ps.pos new_intent in
      desired_movement |> Option.map (fun m -> state, m))
    |> Player_map.filter (fun id (_state, movement_change) ->
      is_valid_position id movement_change.target_position game_state)
  in
  let collisions =
    moving_players |> Player_map.mapi (fun _ (_, m) -> m) |> find_colliding_players
  in
  moving_players
  |> Player_map.filter (fun id _ -> not @@ List.exists (fun (p, _) -> p == id) collisions)
  |> Player_map.iter (fun _id (state, movement_change) ->
    state
    := make_state
         ~pos:movement_change.target_position
         ~intent:movement_change.remaining_intent)
;;

let main_loop renderer (game_state : Game_state.t) =
  let e = Sdl.Event.create () in
  let quit = ref false in
  let tick = ref 0 in
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
    ignore @@ run_tick game_state !tick;
    Sdl.set_render_draw_color renderer ~r:20 ~g:20 ~b:20;
    Sdl.render_clear renderer;
    game_state.players
    |> Player_map.iter (fun _id { state; impl } ->
      let module M = (val impl : Player.PLAYER) in
      draw_player renderer M.meta !state);
    Sdl.render_present renderer;
    Thread.delay 0.01
  done
;;

let main () =
  let state1 = ref @@ make_initial_state { x = 100.; y = 50. } in
  let impl1 = Player.load_lua_player "lloyd.lua" (fun () -> !state1.pos) in
  let state2 = ref @@ make_initial_state { x = 450.; y = 80. } in
  let impl2 = Player.load_lua_player "cole.lua" (fun () -> !state2.pos) in
  let game_state =
    Game_state.initial
    |> Game_state.add_player state1 impl1
    |> Game_state.add_player state2 impl2
  in
  Sdl.with_sdl (fun () ->
    Sdl.with_window_and_renderer
      ~w:arena_width
      ~h:arena_height
      "Arena"
      (fun _window renderer -> main_loop renderer game_state))
;;
