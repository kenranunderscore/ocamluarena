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

let is_dead player_state = player_state.hp <= 0
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
    { living_players : player_data Player_map.t
    ; dead_players : player_data Player_map.t
    ; attacks : attack_state list Player_map.t ref
    }

  let initial =
    { living_players = Player_map.empty
    ; dead_players = Player_map.empty
    ; attacks = ref Player_map.empty
    }
  ;;

  (* find_first: here *)

  let add_player state impl game_state =
    let data = { state; impl } in
    (* NOTE: assumes no players are dead *)
    let new_id = 1 + (Player_map.bindings game_state.living_players |> List.length) in
    { game_state with
      living_players =
        Player_map.add (Player.Id.make new_id) data game_state.living_players
    }
  ;;
end

module Game = struct
  let start_new _player_files =
    (* TODO: implement add_player s.t. ids and state are distributed
       automatically, with initial position and heading being randomized
       (seeded), and based on the arena + players. Therefore the player
       insertion order should perhaps be randomized as well. *)
    let mk_state_reader (r : player_state ref) () =
      let { pos; heading; hp; _ } = !r in
      { Player.pos; heading; hp }
    in
    let state1 = ref @@ make_initial_state { x = 200.; y = 50. } (2. *. Float.pi /. 3.) in
    let lloyd = Player.Lua.load "lloyd.lua" (mk_state_reader state1) in
    let state2 =
      ref @@ make_initial_state { x = 600.; y = 500. } ((2. *. Float.pi) -. 1.)
    in
    let kai = Player.Lua.load "kai.lua" (mk_state_reader state2) in
    let game_state =
      Game_state.initial
      |> Game_state.add_player state1 lloyd
      |> Game_state.add_player state2 kai
    in
    game_state
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
    game_state.living_players
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
  | Hit_by of string
  | Death

type targeted_event =
  | Global_event of event
  | Player_event of Player.Id.t * event

(* TODO: move attacks or players first? *)
let move_players game_state =
  let player_moves =
    game_state.living_players
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
  (* TODO: return events from movement execution (collisions etc.) *)
  game_state.living_players, []
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
            let hits = players_hit attack game_state.living_players in
            if Player_map.is_empty hits
            then { attack with pos } :: atts, evts
            else
              ( atts
              , Player_map.mapi
                  (fun victim_id p ->
                    let _, owner =
                      Player_map.find_first
                        (fun id -> id = attack.owner)
                        game_state.living_players
                    in
                    let module Victim = (val p.impl : PLAYER) in
                    let module Attacker = (val owner.impl : PLAYER) in
                    [ Player_event
                        (attack.owner, Attack_hit (Victim.meta.name, !(p.state).pos))
                    ; Player_event (victim_id, Hit_by Attacker.meta.name)
                    ])
                  hits
                |> Player_map.bindings
                |> List.map snd
                |> List.concat
                |> List.append evts ))
          else acc)
        attacks
        ([], []))
  in
  game_state.attacks := Player_map.map fst attacks_or_events;
  (* FIXME: collect ALL events, with recipients, and the distribute _later_ *)
  Player_map.fold
    (fun _id evts acc -> List.append evts acc)
    (attacks_or_events |> Player_map.map snd)
    []
;;

let create_attacks (game_state : Game_state.t) =
  let new_attacks =
    game_state.living_players
    |> Player_map.filter_map (fun id p ->
      let ps = !(p.state) in
      Option.map
        (fun heading -> { origin = ps.pos; owner = id; pos = ps.pos; heading })
        ps.intent.attack)
  in
  (* TODO: add game_state function to add attacks *)
  game_state.living_players
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
  []
;;

let apply_intents game_state =
  let _new_state, movement_events = move_players game_state in
  let attack_events = transition_attacks game_state in
  (* TODO: need attack event for player; with attack type? I guess so *)
  let attack_fired_events = create_attacks game_state in
  List.concat [ movement_events; attack_events; attack_fired_events ]
;;

let events_to_commands player events =
  (* TODO: ordering? *)
  let module M = (val player : PLAYER) in
  events
  |> List.concat_map (function
    | Tick tick -> M.on_tick tick
    | Enemy_seen (name, pos) -> M.on_enemy_seen name pos
    | Attack_hit (name, pos) -> M.on_attack_hit name pos
    | Hit_by name -> M.on_hit_by name
    | Death ->
      M.on_death ();
      [])
;;

let update_intent player_data commands =
  let state = !(player_data.state) in
  let new_intent = determine_intent state.intent commands in
  player_data.state := { state with intent = new_intent }
;;

(* FIXME: define event order, and sort accordingly *)
let distribute_events tick events (game_state : Game_state.t) =
  let all_events = Global_event (Tick tick) :: events in
  let player_events =
    (* TODO: propagate to all players? see find_first *)
    game_state.living_players
    |> Player_map.mapi (fun id p ->
      ( p
      , List.filter_map
          (function
            | Global_event evt -> Some evt
            | Player_event (id', evt) when id' = id -> Some evt
            | Player_event _ -> None)
          all_events ))
  in
  Player_map.map (fun (p, events) -> p, events_to_commands p.impl events) player_events
;;

let step (game_state : Game_state.t) tick =
  (* TODO: do I _really_ need to have refs to the player_states? couldn't I just
     create the 'get_player_state' closures over the game state instead? *)
  (* If I had that, I could do most of this purely! *)
  let events = apply_intents game_state in
  let followup_events =
    events
    |> List.map (function
      | Global_event _ -> []
      | Player_event (id, Hit_by _) ->
        let victim =
          Player_map.find_first (fun id' -> id' = id) game_state.living_players |> snd
        in
        let state = !(victim.state) in
        let hp = state.hp - 20 in
        victim.state := { state with hp };
        if is_dead !(victim.state) then [ Player_event (id, Death) ] else []
      | Player_event _ -> [])
    |> List.concat
  in
  let new_state =
    List.fold_right
      (fun evt acc ->
        match evt with
        | Player_event (id, Death) ->
          let player =
            Player_map.find_first (fun id' -> id' = id) game_state.living_players |> snd
          in
          { game_state with
            living_players = Player_map.remove id game_state.living_players
          ; dead_players = Player_map.add id player game_state.dead_players
          }
        | _ -> acc)
      followup_events
      game_state
  in
  let all_events = List.append events followup_events in
  let player_commands = distribute_events tick all_events game_state in
  Player_map.iter (fun _id (p, commands) -> update_intent p commands) player_commands;
  new_state
;;
