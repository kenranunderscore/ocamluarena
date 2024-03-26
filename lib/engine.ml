let failwithf f = Printf.ksprintf failwith f
let arena_width = 1000.
let arena_height = 800.
let player_diameter = 50.
let player_radius = player_diameter /. 2.

(* TODO: property of the actual attack *)
let attack_radius = 4.

module type PLAYER = Player.PLAYER

module Player_map = struct
  include Map.Make (Player.Id)

  let difference m1 m2 =
    merge
      (fun _player l r ->
        match l, r with
        | Some x, None -> Some x
        | _ -> None)
      m1
      m2
  ;;
end

type intent =
  { distance : float
  ; angle : float
  ; attack : float option
  }

let default_intent = { distance = 0.0; angle = 0.0; attack = None }

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

type attack_state =
  { pos : Point.t
  ; heading : float
  ; origin : Point.t
  ; owner : Player.Id.t
  }

module Game_state = struct
  (* TODO: get rid of refs everywhere; see FIXME/TODO below *)
  type t =
    { players : player_data Player_map.t
    ; attacks : attack_state list Player_map.t ref
    }

  let initial = { players = Player_map.empty; attacks = ref Player_map.empty }

  let add_player state impl game_state =
    let data = { state; impl } in
    let new_id = 1 + (Player_map.bindings game_state.players |> List.length) in
    { game_state with
      players = Player_map.add (Player.Id.make new_id) data game_state.players
    }
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

let dist_sqr (p1 : Point.t) (p2 : Point.t) =
  let dx = p1.x -. p2.x in
  let dy = p1.y -. p2.y in
  (dx *. dx) +. (dy *. dy)
;;

(* TODO: inline? *)

let dist (p1 : Point.t) (p2 : Point.t) = Float.sqrt (dist_sqr p1 p2)
let circles_intersect (p1 : Point.t) r1 (p2 : Point.t) r2 = dist p1 p2 <= r1 +. r2

(* FIXME: pull out common movement logic *)

let calculate_new_pos (p : Point.t) heading velocity =
  let dx = sin heading *. velocity in
  let dy = -.(cos heading *. velocity) in
  let x, y = p.x +. dx, p.y +. dy in
  Point.make ~x ~y
;;

let calculate_movement (p : Point.t) old_heading intent =
  let max_velocity = 1. in
  let velocity = Float.min intent.distance max_velocity in
  let turn_rate = to_radians 5. in
  let dangle = sign intent.angle *. Float.min turn_rate (Float.abs intent.angle) in
  let angle = if Float.abs intent.angle < turn_rate then 0. else intent.angle +. dangle in
  let heading = old_heading +. dangle in
  let dx = sin heading *. velocity in
  let dy = -.(cos heading *. velocity) in
  let distance = Float.max 0. (intent.distance -. velocity) in
  let x, y = p.x +. dx, p.y +. dy in
  let position = Point.make ~x ~y in
  let remaining_intent = { intent with distance; angle } in
  { intent = remaining_intent; position; heading }
;;

let determine_intent old_intent cmds =
  let apply_cmd intent = function
    | Player.Move distance -> { intent with distance }
    | Turn_right angle -> { intent with angle }
    | Attack heading -> { intent with attack = Some heading }
  in
  List.fold_left apply_cmd old_intent cmds
;;

let players_collide (pos1 : Point.t) pos2 = Point.dist pos1 pos2 <= player_diameter

let inside_arena { Point.x; y } =
  x >= 0. && x <= arena_width && y >= 0. && y <= arena_height
;;

(* TODO: only pass "obstacles" instead of whole state *)
let is_valid_position
  (player_id : Player.Id.t)
  ({ x; y } as p : Point.t)
  (game_state : Game_state.t)
  =
  let r = player_diameter /. 2. in
  let stays_inside_arena =
    x -. r >= 0. && x +. r <= arena_width && y -. r >= 0. && y +. r <= arena_height
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
    let remaining = Player_map.difference to_check colliding in
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
  | Enemy_seen of string * Point.t (* TODO: replace string with enemy information *)
  | Attack_hit of string * Point.t

let move_players game_state =
  let player_moves =
    game_state.players
    |> Player_map.map (fun { state; impl } ->
      let module M = (val impl : PLAYER) in
      let ps = !state in
      let move = calculate_movement ps.pos ps.heading ps.intent in
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

let players_hit attack players =
  players
  |> Player_map.filter (fun id player ->
    let pos = !(player.state).pos in
    id <> attack.owner && circles_intersect pos player_radius attack.pos attack_radius)
;;

let transition_attacks (game_state : Game_state.t) =
  let attacks_or_events =
    !(game_state.attacks)
    |> Player_map.map (fun attacks ->
      List.fold_right
        (fun attack ((atts, evts) as acc) ->
          let velocity = 3. in
          let pos = calculate_new_pos attack.pos attack.heading velocity in
          if inside_arena pos
          then (
            let hits =
              players_hit attack game_state.players |> Player_map.bindings |> List.map snd
            in
            if List.is_empty hits
            then { attack with pos } :: atts, evts
            else
              ( atts
              , List.map
                  (fun p ->
                    let module M = (val p.impl : PLAYER) in
                    Attack_hit (M.meta.name, !(p.state).pos))
                  hits
                |> List.append evts ))
          else acc)
        attacks
        ([], []))
  in
  game_state.attacks := Player_map.map fst attacks_or_events;
  (* FIXME: collect ALL events, with recipients, and the distribute _later_ *)
  attacks_or_events |> Player_map.map snd
;;

let create_attacks (game_state : Game_state.t) =
  let new_attacks =
    game_state.players
    |> Player_map.filter_map (fun id p ->
      let ps = !(p.state) in
      Option.map
        (fun heading -> { origin = ps.pos; owner = id; pos = ps.pos; heading })
        ps.intent.attack)
  in
  (* TODO: add game_state function to add attacks *)
  game_state.players
  |> Player_map.iter (fun _id p ->
    let ps = !(p.state) in
    p.state := { ps with intent = { ps.intent with attack = None } });
  let prev_attacks = !(game_state.attacks) in
  game_state.attacks
  := Player_map.merge
       (fun _id mprev mnew ->
         match mprev, mnew with
         | Some p, Some n -> Some (n :: p)
         | Some p, None -> Some p
         | None, Some n -> Some [ n ]
         | None, None -> None)
       prev_attacks
       new_attacks;
  (* TODO: add "attacking player" events *)
  Player_map.empty
;;

let apply_intents tick game_state =
  let movement_events = move_players game_state in
  let attack_events = transition_attacks game_state in
  (* TODO: need attack event for player; with attack type? I guess so *)
  let _attack_fired_events = create_attacks game_state in
  let events =
    Player_map.merge
      (fun _id ma mb ->
        match ma, mb with
        | Some (p, a), Some b -> Some (p, List.append a b)
        | Some x, None -> Some x
        | None, Some _ -> None (* impossible *)
        | None, None -> None)
      movement_events
      attack_events
  in
  events |> Player_map.map (fun (p, events) -> p, Tick tick :: events)
;;

let events_to_commands player events =
  (* TODO: ordering? *)
  let module M = (val player : PLAYER) in
  events
  |> List.concat_map (function
    | Tick tick -> M.on_tick tick
    | Enemy_seen (name, pos) -> M.on_enemy_seen name pos
    | Attack_hit (name, pos) -> M.on_attack_hit name pos)
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
  { game_state with Game_state.players }
;;
