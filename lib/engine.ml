let failwithf f = Printf.ksprintf failwith f
let arena_width = 1000
let arena_height = 800
let player_diameter = 50.
let player_radius = player_diameter /. 2.

module type PLAYER = Player.PLAYER

module Player_map = Map.Make (Player.Id)

type intent =
  { distance : float
  ; angle : float
  }

let default_intent = { distance = 0.0; angle = 0.0 }

type player_state =
  { pos : Point.t
  ; heading : float
  ; intent : intent
  ; hp : int
  }

let make_initial_state pos heading = { pos; heading; intent = default_intent; hp = 100 }

type player_data =
  { state : player_state ref
  ; impl : (module PLAYER)
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

(* TODO: implement: take the 'latest' (according to event order?) command of
   each type *)
let reduce_commands commands = commands

type movement_change =
  { intent : intent
  ; position : Point.t
  ; heading : float
  }

let to_radians deg = deg *. Float.pi /. 180.
let sign x = if Float.sign_bit x then -1. else 1.

let calculate_new_pos (old : Point.t) old_heading intent =
  let max_velocity = 1. in
  let velocity = Float.min intent.distance max_velocity in
  let turn_rate = to_radians 5. in
  let dangle = sign intent.angle *. Float.min turn_rate (Float.abs intent.angle) in
  let angle = if Float.abs intent.angle < turn_rate then 0. else intent.angle +. dangle in
  let heading = old_heading +. dangle in
  let dx = sin heading *. velocity in
  let dy = -.(cos heading *. velocity) in
  let distance = Float.max 0. (intent.distance -. velocity) in
  let x, y = old.x +. dx, old.y +. dy in
  let position = Point.make ~x ~y in
  let remaining_intent = { distance; angle } in
  { intent = remaining_intent; position; heading }
;;

let determine_intent old_intent cmds =
  let apply_cmd intent = function
    | Player.Move distance -> { intent with distance }
    | Player.Turn_right angle -> { intent with angle }
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
        (fun _p m -> players_collide movement_change.position m.position)
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

(* an engine step consists of:
   - transition: current intents -> new state * events
     [ where do spells etc. fit in here? transition at the same time? after? ]
   - call event handlers -> accumulate and reduce new commands
   - apply commands to determine and set new intent
*)

type event =
  | Tick of int
  | Enemy_seen of string * Point.t

let move_players game_state =
  let player_moves =
    game_state.players
    |> Player_map.map (fun { state; impl } ->
      let module M = (val impl : PLAYER) in
      let ps = !state in
      let move = calculate_new_pos ps.pos ps.heading ps.intent in
      state, move)
    |> Player_map.filter (fun id (_state, move) ->
      is_valid_position id move.position game_state)
  in
  let collisions = player_moves |> Player_map.map snd |> find_colliding_players in
  player_moves
  |> Player_map.filter (fun id _ -> not @@ List.exists (fun (p, _) -> p == id) collisions)
  |> Player_map.iter (fun _id (state, move) ->
    state
    := { !state with pos = move.position; heading = move.heading; intent = move.intent });
  (* FIXME: return events from movement execution (collisions etc.) *)
  game_state.players |> Player_map.map (fun p -> p, [])
;;

let apply_intents tick game_state =
  let movement_events = move_players game_state in
  movement_events |> Player_map.map (fun (p, events) -> p, Tick tick :: events)
;;

let events_to_commands player events =
  (* TODO: ordering? *)
  let module M = (val player : PLAYER) in
  events
  |> List.concat_map (function
    | Tick tick -> M.on_tick tick
    | Enemy_seen (name, pos) -> M.on_enemy_seen name pos)
;;

let update_intent player_data commands =
  let state = !(player_data.state) in
  let new_intent = determine_intent state.intent commands in
  player_data.state := { state with intent = new_intent };
  player_data
;;

let step (game_state : Game_state.t) tick =
  (* TODO: do I _really_ need to have refs to the player_states? couldn't I just
     create the 'get_player_state' closures over the game state instead? *)
  (* If I had that, I could do most of this purely! *)
  let players =
    game_state
    |> apply_intents tick
    |> Player_map.map (fun (p, events) -> p, events_to_commands p.impl events)
    |> Player_map.map (fun (p, commands) -> update_intent p commands)
  in
  { Game_state.players }
;;
