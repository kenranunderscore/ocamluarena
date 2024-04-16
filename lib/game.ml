let failwithf f = Printf.ksprintf failwith f

(* TODO: property of the actual attack *)
let attack_radius = 4.
let attack_cooldown = 35

type player_data =
  { player_state : Player_state.t
  ; impl : Player.impl
  }

(* TODO: newtypes? records or single-constructor variants? *)
type heading = float
type attack_id = int

type attack_state =
  { pos : Point.t
  ; heading : heading
  ; origin : Point.t
  ; owner : Player.t
  }

module Int_map = Map.Make (Int)

type stats = { wins : int }

let rec first_with f pred =
  let res = f () in
  if pred res then res else first_with f pred
;;

let shuffle xs =
  (* TODO: compare good? *)
  xs |> List.map (fun x -> Random.bits (), x) |> List.sort compare |> List.map snd
;;

type round_state =
  | Ongoing
  | Won of Player.t
  | Draw

module State = struct
  type t =
    { all_players : player_data Players.t
        (* FIXME: players should not be part of state, but rather "config", or
           just closed over *)
    ; players : Player.impl Players.t
    ; attacks : attack_state Int_map.t
    ; round : int
    ; tick : int
    ; round_state : round_state
    }

  let get_player id state = Players.find id state.all_players

  let living_players state =
    Players.filter
      (fun _player data -> Player_state.is_alive data.player_state)
      state.all_players
  ;;

  let update_all_players f state = { state with all_players = f state.all_players }
  let map_players f state = update_all_players (Players.mapi f) state

  let map_living_players f state =
    update_all_players
      (Players.mapi (fun player data ->
         if Player_state.is_alive data.player_state then f player data else data))
      state
  ;;

  let update_player player f state =
    update_all_players
      (Players.update player (function
        | Some data -> Some { data with player_state = f data.player_state }
        | None -> failwith "player should exist"))
      state
  ;;

  let add_attack id attack state =
    { state with attacks = Int_map.add id attack state.attacks }
  ;;

  let remove_attack id state = { state with attacks = Int_map.remove id state.attacks }

  let update_attack id f state =
    { state with
      attacks =
        Int_map.update
          id
          (function
            | Some attack -> Some (f attack)
            | None -> failwith "attack should exist")
          state.attacks
    }
  ;;
end

type t =
  { state : State.t
  ; settings : Settings.t
  }

let update_state_with_settings f game = { game with state = f game.settings game.state }
let update_state f game = { game with state = f game.state }

let make_reader (player : Player.t) game_ref () =
  let player = State.get_player player !game_ref.state in
  let { Player_state.pos; heading; hp; view_direction; _ } = player.player_state in
  { Player.pos; heading; hp; view_direction }
;;

let add_player ((player : Player.t), directory) game_ref =
  let game = !game_ref in
  let state = game.state in
  let path = Filename.concat directory "main.lua" in
  let impl =
    Player.Lua.load_implementation path player.name (make_reader player game_ref)
  in
  game_ref
  := game
     |> update_state (fun s -> { s with players = Players.add player impl state.players });
  game_ref
;;

let init (settings : Settings.t) players =
  let seed = settings.rng_seed in
  Random.init seed;
  Printf.printf
    "Initializing new %i-round game with random seed %i\n%!"
    settings.rounds
    seed;
  List.fold_right
    add_player
    players
    (ref
       { settings
       ; state =
           { all_players = Players.empty
           ; players = Players.empty
           ; attacks = Int_map.empty
           ; round = 0
           ; tick = 0
           ; round_state = Ongoing
           }
       })
;;

let random_initial_player_state (settings : Settings.t) (state : State.t) =
  let diameter = 2. *. settings.player_radius in
  let random_coord dim = dim -. (4. *. diameter) |> Random.float |> ( +. ) diameter in
  let random_pos () =
    Point.make
      ~x:(random_coord settings.arena_width)
      ~y:(random_coord settings.arena_height)
  in
  let is_valid p =
    not
      (State.living_players state
       |> Players.exists (fun _player { player_state = s; _ } ->
         Point.dist p s.pos < 2. *. diameter))
  in
  let pos = first_with random_pos is_valid in
  let heading = Random.float Math.two_pi in
  { Player_state.pos
  ; heading
  ; view_direction = 0.
  ; hp = 100
  ; intent = Intent.default
  ; attack_cooldown = 0
  }
;;

let players_collide player_diameter (pos1 : Point.t) pos2 =
  Point.dist pos1 pos2 <= player_diameter
;;

let inside_arena ~arena_width ~arena_height { Point.x; y } =
  Math.is_between x 0. arena_width && Math.is_between y 0. arena_height
;;

let reduce_commands commands =
  let rec reduce = function
    | [] -> []
    | h :: t ->
      h
      :: (reduce
          @@ List.filter (fun x -> Player.command_index x <> Player.command_index h) t)
  in
  reduce (List.rev commands)
;;

type movement_change =
  { remaining_movement_intent : Intent.Movement.t
  ; remaining_turn_angle : float
  ; position : Point.t
  ; heading : heading
  }

(* TODO: pull out common movement logic *)
let calculate_new_pos (p : Point.t) heading velocity =
  let dx = sin heading *. velocity in
  let dy = -.(cos heading *. velocity) in
  let x, y = p.x +. dx, p.y +. dy in
  Point.make ~x ~y
;;

let calculate_movement max_turn_rate (p : Point.t) old_heading (intent : Intent.t) =
  let max_velocity = 1. in
  let velocity = Float.min intent.movement.distance max_velocity in
  let dangle =
    Math.sign intent.turn_angle *. Float.min max_turn_rate (Float.abs intent.turn_angle)
  in
  let turn_angle =
    if Float.abs intent.turn_angle < max_turn_rate
    then 0.
    else intent.turn_angle +. dangle
  in
  let heading = Math.normalize_absolute_angle (old_heading +. dangle) in
  let dir_heading =
    match intent.movement.direction with
    | Player.Forward -> 0.
    | Backward -> Float.pi
    | Left -> -.Math.half_pi
    | Right -> Math.half_pi
  in
  let movement_heading = heading +. dir_heading in
  let distance = Float.max 0. (intent.movement.distance -. velocity) in
  let movement_intent =
    { Intent.Movement.distance; direction = intent.movement.direction }
  in
  let position = calculate_new_pos p movement_heading velocity in
  { remaining_movement_intent = movement_intent
  ; remaining_turn_angle = turn_angle
  ; position
  ; heading
  }
;;

let determine_intent old_intent cmds =
  let apply_cmd intent = function
    | Player.Move (direction, distance) ->
      Intent.set_movement intent { Intent.Movement.distance; direction }
    | Turn_right turn_angle -> { intent with Intent.turn_angle }
    | Attack heading -> { intent with attack = Some heading }
    | Look_right view_angle -> { intent with view_angle }
  in
  List.fold_left apply_cmd old_intent cmds
;;

(* TODO: only pass "obstacles" instead of whole state *)
let is_valid_position
  ~arena_width
  ~arena_height
  player_radius
  (player : Player.t)
  ({ x; y } as p : Point.t)
  (state : State.t)
  =
  let stays_inside_arena =
    x -. player_radius >= 0.
    && x +. player_radius <= arena_width
    && y -. player_radius >= 0.
    && y +. player_radius <= arena_height
  in
  let would_hit_other_player =
    State.living_players state
    |> Players.exists (fun other_player { player_state; _ } ->
      player <> other_player && players_collide (2. *. player_radius) p player_state.pos)
  in
  stays_inside_arena && not would_hit_other_player
;;

let find_colliding_players player_diameter positions =
  let rec go (_player, movement_change) to_check acc =
    (* TODO: find out who collided with whom -> event *)
    let colliding =
      Players.filter
        (fun _p m -> players_collide player_diameter movement_change.position m.position)
        to_check
    in
    let new_acc = List.append acc (Players.bindings colliding) in
    let remaining = Players.difference to_check colliding in
    if Players.is_empty remaining
    then new_acc
    else (
      let next = Players.choose remaining in
      go next (Players.remove (fst next) remaining) new_acc)
  in
  match Players.choose_opt positions with
  | Some first ->
    let others = Players.remove (fst first) positions in
    go first others []
  | None -> []
;;

module Event = struct
  type t =
    | Round_started of int
    | Round_over of Player.t option
    | Tick of int
    | Hit of attack_id * Player.t * Player.t * Point.t
    | Attack_missed of attack_id
    | Attack_advanced of attack_id * Point.t
    | Attack_created of attack_id * attack_state
    | Head_turned of Player.t * float * float
    | Player_moved of Player.t * movement_change

  let index = function
    | Round_started _ -> -2
    | Round_over _ -> -1
    | Tick _ -> 0
    | Hit _ -> 1
    | Attack_missed _ -> 2
    | Attack_advanced _ -> 3
    | Attack_created _ -> 4
    | Head_turned _ -> 5
    | Player_moved _ -> 6
  ;;

  let compare e1 e2 = Int.compare (index e1) (index e2)
end

module Player_event = struct
  type t =
    | Tick of int
    | Round_started of int
    | Enemy_seen of string * Point.t
    | Attack_hit of string * Point.t
    | Hit_by of string
    | Round_over of Player.t option
    | Round_won
    | Death

  let index = function
    | Tick _ -> 0
    | Round_started _ -> 1
    | Enemy_seen _ -> 2
    | Attack_hit _ -> 3
    | Hit_by _ -> 4
    | Round_over _ -> 5
    | Round_won -> 6
    | Death -> 7
  ;;

  let compare e1 e2 = Int.compare (index e1) (index e2)
end

let turn_head player ~max_view_turn_rate ~view_direction ~angle =
  if angle <> 0.
  then (
    let abs_angle = Float.abs angle in
    let dangle = Math.sign angle *. Float.min max_view_turn_rate abs_angle in
    let new_view_direction =
      Math.clamp (view_direction +. dangle) (-.Math.half_pi) Math.half_pi
    in
    let remaining_angle =
      Math.clamp (view_direction +. angle) (-.Math.half_pi) Math.half_pi
      -. new_view_direction
    in
    let event = Event.Head_turned (player, new_view_direction, remaining_angle) in
    Some event)
  else None
;;

(* TODO: decide:
   - should this return vision events, or should these be decided AFTER movement?
   - or should this just be called after movement _and_ return vision events?
*)
let turn_heads max_view_turn_rate (state : State.t) =
  let players = State.living_players state in
  Players.fold
    (fun player data events ->
      let { Player_state.view_direction; intent; _ } = data.player_state in
      List.append
        (turn_head player ~max_view_turn_rate ~view_direction ~angle:intent.view_angle
         |> Option.to_list)
        events)
    players
    []
;;

(* TODO: move attacks or players first? *)
let move_players
  ({ arena_width; arena_height; max_turn_rate; player_radius; _ } : Settings.t)
  (state : State.t)
  =
  let player_diameter = 2. *. player_radius in
  let player_moves =
    state
    |> State.living_players
    |> Players.filter_map (fun player data ->
      let move =
        calculate_movement
          max_turn_rate
          data.player_state.pos
          data.player_state.heading
          data.player_state.intent
      in
      if is_valid_position
           ~arena_width
           ~arena_height
           player_radius
           player
           move.position
           state
      then Some move
      else None)
  in
  let collisions = player_moves |> find_colliding_players player_diameter in
  (* TODO: return events from movement execution (collisions etc.) *)
  player_moves
  |> Players.filter (fun player _ ->
    not @@ List.exists (fun (other_player, _) -> player = other_player) collisions)
  |> Players.bindings
  |> List.map (fun (id, move) -> Event.Player_moved (id, move))
;;

let players_hit player_radius owner pos players =
  players
  |> Players.filter (fun player data ->
    player <> owner
    && Math.circles_intersect data.player_state.pos player_radius pos attack_radius)
;;

let transition_attacks
  ({ arena_width; arena_height; player_radius; _ } : Settings.t)
  (state : State.t)
  =
  Int_map.fold
    (fun id attack events ->
      let velocity = 3. in
      let pos = calculate_new_pos attack.pos attack.heading velocity in
      if inside_arena ~arena_width ~arena_height pos
      then (
        let hits =
          players_hit player_radius attack.owner pos (State.living_players state)
        in
        if Players.is_empty hits
        then Event.Attack_advanced (id, pos) :: events
        else
          Players.fold
            (fun victim _p acc -> Event.Hit (id, attack.owner, victim, pos) :: acc)
            hits
            events)
      else Event.Attack_missed id :: events)
    state.attacks
    []
;;

let create_attacks (state : State.t) =
  (* TODO: add "attacking player" events *)
  state
  |> State.living_players
  |> Players.filter_map (fun player data ->
    match data.player_state.intent.attack with
    | Some heading when data.player_state.attack_cooldown = 0 ->
      let attack_id = 1 + Int_map.fold (fun id _ acc -> max acc id) state.attacks 0 in
      let attack =
        { origin = data.player_state.pos
        ; owner = player
        ; pos = data.player_state.pos
        ; heading
        }
      in
      Some (Event.Attack_created (attack_id, attack))
    | Some _ | None -> None)
  |> Players.values
;;

let tick_events (state : State.t) =
  let events = [ Event.Tick state.tick ] in
  if state.tick = 0 then Event.Round_started state.round :: events else events
;;

let round_over (state : State.t) = Players.cardinal (State.living_players state) <= 1
let round_winner (state : State.t) = Players.choose_opt (State.living_players state)

let check_for_round_end = function
  | state when round_over state ->
    [ (match round_winner state with
       | Some (id, _) -> Event.Round_over (Some id)
       | None -> Round_over None)
    ]
  | _ -> []
;;

let determine_game_events (settings : Settings.t) (state : State.t) =
  List.concat
    [ tick_events state
    ; turn_heads settings.max_view_turn_rate state
    ; move_players settings state
    ; transition_attacks settings state
    ; create_attacks state
    ; check_for_round_end state
    ]
  |> List.sort Event.compare
;;

let can_spot ~player_angle_of_vision ~player_radius p view_direction q =
  let dangle = player_angle_of_vision /. 2. in
  let left = view_direction -. dangle in
  let right = view_direction +. dangle in
  let d = Point.dist p q in
  let alpha = Float.atan (player_radius /. d) in
  let angle = Math.normalize_absolute_angle (Math.angle_between p q) in
  let alpha_left = angle -. alpha in
  let alpha_right = angle +. alpha in
  Math.is_between alpha_left left right
  || Math.is_between alpha_right left right
  || (alpha_left <= left && alpha_right >= right)
;;

let process_game_events events _settings game_state =
  List.fold_left
    (fun (state : State.t) -> function
      | Event.Round_started _round -> state
      | Round_over (Some id) -> { state with round_state = Won id }
      | Round_over None -> { state with round_state = Draw }
      | Tick tick ->
        (* TODO: don't hold the cooldown information, rather the tick when
           attacking is possible again *)
        { state with tick = tick + 1 }
        |> State.map_living_players (fun _player data ->
          let player_state =
            { data.player_state with
              attack_cooldown = max 0 (data.player_state.attack_cooldown - 1)
            }
          in
          { data with player_state })
      | Hit (id, _owner, victim, _p) ->
        state
        |> State.update_player victim (fun s -> { s with hp = s.hp - 20 })
        |> State.remove_attack id
      (* TODO: utilities for working with attacks in State.t *)
      | Attack_missed id -> State.remove_attack id state
      | Attack_advanced (id, pos) ->
        State.update_attack id (fun attack -> { attack with pos }) state
      | Attack_created (id, attack) ->
        state
        |> State.add_attack id attack
        |> State.update_player attack.owner (fun p ->
          let intent = { p.intent with attack = None } in
          { p with intent; attack_cooldown })
      | Head_turned (id, view_direction, view_angle) ->
        State.update_player
          id
          (fun p ->
            let intent = { p.intent with view_angle } in
            { p with view_direction; intent })
          state
      | Player_moved (id, move) ->
        State.update_player
          id
          (fun p ->
            let intent =
              { p.intent with
                turn_angle = move.remaining_turn_angle
              ; movement = move.remaining_movement_intent
              }
            in
            { p with pos = move.position; heading = move.heading; intent })
          state)
    game_state
    events
;;

(* FIXME: player_event -> into player.ml? *)
let read_player_commands (impl : Player.impl) player_events =
  player_events
  |> List.concat_map (function
    | Player_event.Tick n -> impl.on_tick n
    | Enemy_seen (enemy, pos) -> impl.on_enemy_seen enemy pos
    | Attack_hit (enemy, pos) -> impl.on_attack_hit enemy pos
    | Hit_by enemy -> impl.on_hit_by enemy
    | Round_started n -> impl.on_round_started n
    | Round_over mwinner ->
      impl.on_round_over (Option.map (fun p -> Player.(p.name)) mwinner);
      []
    | Round_won ->
      impl.on_round_won ();
      []
    | Death ->
      impl.on_death ();
      [])
  |> reduce_commands
;;

let update_intent player_data commands =
  let state = player_data.player_state in
  let new_intent = determine_intent state.intent commands in
  { player_data with player_state = { state with intent = new_intent } }
;;

let player_events_from_game_events player player_state game_events =
  List.fold_left
    (fun acc -> function
      | Event.Round_started round -> Player_event.Round_started round :: acc
      | Round_over (Some id) when id = player -> Player_event.Round_won :: acc
      | Round_over mwinner -> Player_event.Round_over mwinner :: acc
      | Tick tick -> Player_event.Tick tick :: acc
      | Hit (_, owner, victim, pos) when player = owner ->
        Player_event.Attack_hit (victim.name, pos) :: acc
      | Hit (_, owner, victim, _pos) when player = victim ->
        let acc = Player_event.Hit_by owner.name :: acc in
        (* TODO: deduplicate *)
        if Player_state.is_dead player_state then Player_event.Death :: acc else acc
      | Hit _ -> acc
      | Attack_missed _ | Attack_created _ | Attack_advanced _ -> acc
      | Head_turned _ | Player_moved _ -> acc)
    []
    game_events
;;

(* TODO: vision events should probably be based on the state before any
   movement? *)
let distribute_player_events game_events settings state =
  let { Settings.player_angle_of_vision; player_radius; _ } = settings in
  State.map_players
    (fun meta p ->
      let visible_players =
        State.living_players state
        |> Players.filter (fun other_meta other_p ->
          meta <> other_meta
          && can_spot
               ~player_angle_of_vision
               ~player_radius
               p.player_state.pos
               (Player_state.resulting_view_direction p.player_state)
               other_p.player_state.pos)
      in
      let enemy_seen_events =
        visible_players
        |> Players.mapi (fun target_meta target ->
          Player_event.Enemy_seen (target_meta.name, target.player_state.pos))
        |> Players.values
      in
      let player_events =
        game_events
        |> player_events_from_game_events meta p.player_state
        |> List.append enemy_seen_events
        |> List.sort Player_event.compare
      in
      read_player_commands p.impl player_events |> update_intent p)
    state
;;

let step game =
  (* TODO: think about order of "things that must happen" with tick 0 in mind *)
  let game_events = determine_game_events game.settings game.state in
  game
  |> update_state_with_settings (process_game_events game_events)
  |> update_state_with_settings (distribute_player_events game_events)
;;

let init_round round settings (state : State.t) =
  List.fold_right
    (fun (id, impl) s ->
      let player_state = random_initial_player_state settings s in
      let data = { player_state; impl } in
      { s with all_players = Players.add id data s.all_players })
    (state.players |> Players.bindings |> shuffle)
    { state with tick = 0; round; all_players = Players.empty; round_state = Ongoing }
;;

let run game_ref =
  let rec run_round () =
    game_ref := step !game_ref;
    match !game_ref.state.round_state with
    | Ongoing ->
      Thread.delay 0.008;
      run_round ()
    | Draw ->
      print_endline "-- DRAW --";
      Thread.delay 2.
    | Won player ->
      Printf.printf "-- WINNER: %s --\n%!" player.name;
      Thread.delay 2.
  in
  let rec go round =
    let game = !game_ref in
    if round > game.settings.rounds
    then print_endline "== GAME OVER =="
    else (
      Printf.printf "Round %i starting...\n%!" round;
      game_ref := update_state_with_settings (init_round round) !game_ref;
      run_round ();
      go (round + 1))
  in
  go 1
;;
