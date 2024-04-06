let failwithf f = Printf.ksprintf failwith f

(* TODO: property of the actual attack *)
let attack_radius = 4.
let attack_cooldown = 35

module type PLAYER = Player.PLAYER

(* TODO: hide the map implementation *)
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

  let values m = bindings m |> List.map snd
end

type movement_intent =
  { distance : float
  ; direction : Player.movement_direction
  }

let default_movement_intent = { distance = 0.0; direction = Forward }

type intent =
  { movement : movement_intent
  ; turn_angle : float
  ; view_angle : float
  ; attack : float option
  }

let default_intent =
  { movement = default_movement_intent
  ; turn_angle = 0.0
  ; view_angle = 0.0
  ; attack = None
  }
;;

type player_state =
  { pos : Point.t
  ; heading : float
  ; view_direction : float
  ; intent : intent
  ; attack_cooldown : int
  ; hp : int
  }

let is_alive player_state = player_state.hp > 0
let is_dead player_state = not (is_alive player_state)

type player_data =
  { player_state : player_state
  ; impl : (module PLAYER)
  }

type attack_state =
  { pos : Point.t
  ; heading : float
  ; origin : Point.t
  ; owner : Player.Id.t
  }

type stats = { wins : int }

let rec first_with f pred =
  let res = f () in
  if pred res then res else first_with f pred
;;

let shuffle xs =
  xs |> List.map (fun x -> Random.bits (), x) |> List.sort compare |> List.map snd
;;

module State = struct
  type t =
    { all_players : player_data Player_map.t
        (* FIXME: players and settings should not be part of state, but rather
           "config" *)
    ; players : (module PLAYER) Player_map.t
    ; stats : stats Player_map.t
    ; attacks : attack_state list Player_map.t
    ; round : int
    }

  let get_player id state = Player_map.find id state.all_players

  let living_players state =
    Player_map.filter (fun _id p -> is_alive p.player_state) state.all_players
  ;;

  let update_all_players f state = { state with all_players = f state.all_players }

  let map_living_players f state =
    update_all_players
      (Player_map.mapi (fun id p -> if is_alive p.player_state then f id p else p))
      state
  ;;

  let update_living_player id f state =
    update_all_players (Player_map.update id (Option.map f)) state
  ;;
end

type t =
  { state : State.t
  ; settings : Settings.t
  }

let make_reader (id : Player.Id.t) game_ref () =
  let player = State.get_player id !game_ref.state in
  let { pos; heading; hp; view_direction; _ } = player.player_state in
  { Player.pos; heading; hp; view_direction }
;;

let add_player player_file game_ref =
  let game = !game_ref in
  let state = game.state in
  let new_id = 1 + (Player_map.bindings state.players |> List.length) |> Player.Id.make in
  let impl = Player.Lua.load player_file (make_reader new_id game_ref) in
  game_ref
  := { game with
       state =
         { state with
           players = Player_map.add new_id impl state.players
         ; stats = Player_map.add new_id { wins = 0 } state.stats
         }
     };
  game_ref
;;

let init (settings : Settings.t) =
  let seed = settings.rng_seed in
  Random.init seed;
  Printf.printf
    "Initializing new %i-round game with random seed %i\n%!"
    settings.rounds
    seed;
  List.fold_right
    add_player
    settings.player_files
    (ref
       { settings
       ; state =
           { all_players = Player_map.empty
           ; players = Player_map.empty
           ; stats = Player_map.empty
           ; attacks = Player_map.empty
           ; round = 0
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
       |> Player_map.exists (fun _id { player_state = s; _ } ->
         Point.dist p s.pos < 2. *. diameter))
  in
  let pos = first_with random_pos is_valid in
  let heading = Random.float Math.two_pi in
  let view_direction = Random.float Math.two_pi in
  { pos; heading; view_direction; hp = 100; intent = default_intent; attack_cooldown = 0 }
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
  { intent : intent
  ; position : Point.t
  ; heading : float
  }

(* TODO: pull out common movement logic *)
let calculate_new_pos (p : Point.t) heading velocity =
  let dx = sin heading *. velocity in
  let dy = -.(cos heading *. velocity) in
  let x, y = p.x +. dx, p.y +. dy in
  Point.make ~x ~y
;;

let calculate_movement max_turn_rate (p : Point.t) old_heading intent =
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
  let heading = old_heading +. dangle in
  let dir_heading =
    match intent.movement.direction with
    | Player.Forward -> 0.
    | Backward -> Float.pi
    | Left -> -.Math.half_pi
    | Right -> Math.half_pi
  in
  let movement_heading = heading +. dir_heading in
  let dx = sin movement_heading *. velocity in
  let dy = -.(cos movement_heading *. velocity) in
  let distance = Float.max 0. (intent.movement.distance -. velocity) in
  let movement_intent = { distance; direction = intent.movement.direction } in
  let x, y = p.x +. dx, p.y +. dy in
  let position = Point.make ~x ~y in
  let remaining_intent = { intent with movement = movement_intent; turn_angle } in
  { intent = remaining_intent; position; heading }
;;

let determine_intent old_intent cmds =
  let apply_cmd intent = function
    | Player.Move (direction, distance) ->
      { intent with movement = { distance; direction } }
    | Turn_right turn_angle -> { intent with turn_angle }
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
  (player_id : Player.Id.t)
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
    |> Player_map.exists (fun id { player_state; _ } ->
      player_id <> id && players_collide player_radius p player_state.pos)
  in
  stays_inside_arena && not would_hit_other_player
;;

let find_colliding_players player_diameter positions =
  let rec go (_player, movement_change) to_check acc =
    (* TODO: find out who collided with whom -> event *)
    let colliding =
      Player_map.filter
        (fun _p m -> players_collide player_diameter movement_change.position m.position)
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
   - transition: state * current intents -> new state * events
     [ where do spells etc. fit in here? transition at the same time? after? ]
   - call event handlers -> accumulate and reduce new commands
   - apply commands to determine and set new intent
*)

type event =
  | Round_started of int
  | Tick of int
  | Enemy_seen of string * Point.t (* TODO: replace string with enemy information *)
  | Attack_hit of string * Point.t
  | Hit_by of string
  | Death
  | Round_over of string option
  | Round_won

let event_index = function
  | Round_started _ -> -1
  | Tick _ -> 0
  | Enemy_seen _ -> 1
  | Attack_hit _ -> 2
  | Hit_by _ -> 3
  | Death -> 4
  | Round_over _ -> 5
  | Round_won -> 6
;;

type targeted_event =
  | Global_event of event
  | Player_event of Player.Id.t * event

(* TODO: decide:
   - should this return vision events, or should these be decided AFTER movement?
   - or should this just be called after movement _and_ return vision events?
*)
let move_heads (settings : Settings.t) (state : State.t) =
  let result =
    state
    |> State.map_living_players (fun _id p ->
      let intent = p.player_state.intent in
      let abs_angle = Float.abs intent.view_angle in
      let dangle =
        Math.sign intent.view_angle *. Float.min settings.max_view_turn_rate abs_angle
      in
      let angle =
        if abs_angle < settings.max_view_turn_rate
        then 0.
        else intent.view_angle +. dangle
      in
      let intent = { intent with view_angle = angle } in
      (* TODO: modify/set_FOO functions on state *)
      let view_direction =
        Math.normalize_absolute_angle (p.player_state.view_direction +. dangle)
      in
      { p with player_state = { p.player_state with intent; view_direction } })
  in
  result, []
;;

(* TODO: move attacks or players first? *)
(* TODO: stamp coupling: refine? *)
let move_players
  ({ arena_width; arena_height; max_turn_rate; player_radius; _ } : Settings.t)
  (state : State.t)
  =
  let player_diameter = 2. *. player_radius in
  let player_moves =
    state
    |> State.living_players
    |> Player_map.filter_map (fun id p ->
      let move =
        calculate_movement
          max_turn_rate
          p.player_state.pos
          p.player_state.heading
          p.player_state.intent
      in
      if is_valid_position ~arena_width ~arena_height player_radius id move.position state
      then Some move
      else None)
  in
  let collisions = player_moves |> find_colliding_players player_diameter in
  let moves_to_execute =
    player_moves
    |> Player_map.filter (fun id _ ->
      not @@ List.exists (fun (id', _) -> id' = id) collisions)
  in
  let res =
    State.map_living_players
      (fun id p ->
        match Player_map.find_opt id moves_to_execute with
        | Some move ->
          { p with
            player_state =
              { p.player_state with
                pos = move.position
              ; heading = move.heading
              ; intent = move.intent
              }
          }
        | None -> p)
      state
  in
  (* TODO: return events from movement execution (collisions etc.) *)
  res, []
;;

let players_hit player_radius attack players =
  players
  |> Player_map.filter (fun id player ->
    id <> attack.owner
    && Math.circles_intersect
         player.player_state.pos
         player_radius
         attack.pos
         attack_radius)
;;

let transition_attacks
  ({ arena_width; arena_height; player_radius; _ } : Settings.t)
  (state : State.t)
  =
  let attacks_or_events =
    state.attacks
    |> Player_map.map (fun attacks ->
      List.fold_right
        (fun attack ((atts, evts) as acc) ->
          let velocity = 3. in
          let pos = calculate_new_pos attack.pos attack.heading velocity in
          if inside_arena ~arena_width ~arena_height pos
          then (
            let hits = players_hit player_radius attack (State.living_players state) in
            if Player_map.is_empty hits
            then { attack with pos } :: atts, evts
            else
              ( atts
              , Player_map.mapi
                  (fun victim_id p ->
                    let owner = State.get_player attack.owner state in
                    let module Victim = (val p.impl : PLAYER) in
                    let module Attacker = (val owner.impl : PLAYER) in
                    [ Player_event
                        (attack.owner, Attack_hit (Victim.meta.name, p.player_state.pos))
                    ; Player_event (victim_id, Hit_by Attacker.meta.name)
                    ])
                  hits
                |> Player_map.values
                |> List.concat
                |> List.append evts ))
          else acc)
        attacks
        ([], []))
  in
  ( { state with attacks = Player_map.map fst attacks_or_events }
  , Player_map.fold
      (fun _id evts acc -> List.append evts acc)
      (attacks_or_events |> Player_map.map snd)
      [] )
;;

let create_attacks (state : State.t) =
  let players_with_attacks =
    state
    |> State.living_players
    |> Player_map.mapi (fun id p ->
      match p.player_state.intent.attack with
      | Some heading when p.player_state.attack_cooldown = 0 ->
        let attack =
          { origin = p.player_state.pos; owner = id; pos = p.player_state.pos; heading }
        in
        ( { p with
            player_state =
              { p.player_state with
                intent = { p.player_state.intent with attack = None }
              ; attack_cooldown
              }
          }
        , Some attack )
      | Some _ | None ->
        ( { p with
            player_state =
              { p.player_state with
                attack_cooldown = Int.max 0 (p.player_state.attack_cooldown - 1)
              }
          }
        , None ))
  in
  (* TODO: add "attacking player" events *)
  (* TODO: add state function to add attacks *)
  let new_attacks = players_with_attacks |> Player_map.filter_map (fun _id x -> snd x) in
  let updated_players = players_with_attacks |> Player_map.map fst in
  let attacks =
    Player_map.merge
      (fun _id mprev mnew ->
        match mprev, mnew with
        | Some p, Some n -> Some (n :: p)
        | Some p, None -> Some p
        | None, Some n -> Some [ n ]
        | None, None -> None)
      state.attacks
      new_attacks
  in
  { state with attacks }
  |> State.update_all_players (fun ps ->
    Player_map.merge
      (fun _id ml mr ->
        match ml, mr with
        | Some _, Some r -> Some r
        | Some l, None -> Some l
        | None, Some _ | None, None -> failwith "impossible")
      ps
      updated_players)
  |> fun s -> s, []
;;

let apply_intents (settings : Settings.t) state =
  (* TODO: refactor *)
  let state, vision_events = move_heads settings state in
  let state, movement_events = move_players settings state in
  let state, attack_events = transition_attacks settings state in
  (* TODO: need attack event for player; with attack type? I guess so *)
  let state, attack_fired_events = create_attacks state in
  ( state
  , List.concat [ vision_events; movement_events; attack_events; attack_fired_events ] )
;;

(* TODO: invariant:

   in on_enemy_seen, make sure the data returned by me.pos(), me.x(),
   me.heading() etc. is from the same tick as the enemy information *)
let events_to_commands player events =
  let module M = (val player : PLAYER) in
  events
  |> List.concat_map (function
    | Round_started round -> M.on_round_started round
    | Tick tick -> M.on_tick tick
    | Enemy_seen (name, pos) -> M.on_enemy_seen name pos
    | Attack_hit (name, pos) -> M.on_attack_hit name pos
    | Hit_by name -> M.on_hit_by name
    | Death ->
      M.on_death ();
      []
    | Round_over mp ->
      M.on_round_over mp;
      []
    | Round_won ->
      M.on_round_won ();
      [])
;;

let update_intent player_data commands =
  let state = player_data.player_state in
  let new_intent = determine_intent state.intent commands in
  { player_data with player_state = { state with intent = new_intent } }
;;

let sort_events events =
  let comp e1 e2 = compare (event_index e1) (event_index e2) in
  events |> List.sort comp
;;

let distribute_events tick events (state : State.t) =
  let events = Global_event (Tick tick) :: events in
  let all_events =
    if tick = 0 then Global_event (Round_started state.round) :: events else events
  in
  state
  |> State.map_living_players (fun id p ->
    all_events
    |> List.filter_map (function
      | Global_event evt -> Some evt
      | Player_event (id', evt) when id' = id -> Some evt
      | Player_event _ -> None)
    |> sort_events
    |> events_to_commands p.impl
    |> reduce_commands
    |> update_intent p)
;;

let transition_hitpoints events state =
  let state =
    List.fold_right
      (fun evt (acc : State.t) ->
        match evt with
        | Global_event _ -> acc
        | Player_event (id, Hit_by _) ->
          State.update_living_player
            id
            (fun p ->
              let hp = p.player_state.hp - 20 in
              let player_state = { p.player_state with hp } in
              { p with player_state })
            acc
        | Player_event _ -> acc)
      events
      state
  in
  (* TODO: get state and events as single step *)
  let death_events =
    state
    |> State.living_players
    |> Player_map.filter_map (fun id p ->
      if is_dead p.player_state then Some (Player_event (id, Death)) else None)
    |> Player_map.values
  in
  state, death_events
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

let vision_events
  ({ player_angle_of_vision; player_radius; _ } : Settings.t)
  (state : State.t)
  =
  let living_players = State.living_players state in
  living_players
  |> Player_map.mapi (fun id p ->
    living_players
    |> Player_map.filter_map (fun id' p' ->
      if id <> id'
         && can_spot
              ~player_angle_of_vision
              ~player_radius
              p.player_state.pos
              p.player_state.view_direction
              p'.player_state.pos
      then
        let module M = (val p'.impl : PLAYER) in
        Some (Player_event (id, Enemy_seen (M.meta.name, p'.player_state.pos)))
      else None)
    |> Player_map.values)
  |> Player_map.values
  |> List.concat
;;

let round_over (state : State.t) = Player_map.cardinal (State.living_players state) <= 1
let round_winner (state : State.t) = Player_map.choose_opt (State.living_players state)

let step game tick =
  (* smells like state monad *)
  let state = game.state in
  let enemy_seen_events = vision_events game.settings state in
  let state, intent_events = apply_intents game.settings state in
  let state, death_events = transition_hitpoints intent_events state in
  let round_over_events =
    if round_over state
    then (
      match round_winner state with
      | Some (id, winner) ->
        let module M = (val winner.impl : PLAYER) in
        [ Global_event (Round_over (Some M.meta.name)); Player_event (id, Round_won) ]
      | None -> [ Global_event (Round_over None) ])
    else []
  in
  let all_events =
    List.concat [ intent_events; death_events; enemy_seen_events; round_over_events ]
  in
  distribute_events tick all_events state
;;

let init_round round (settings : Settings.t) (state : State.t) =
  List.fold_right
    (fun (id, impl) s ->
      let player_state = random_initial_player_state settings s in
      let data = { player_state; impl } in
      { s with all_players = Player_map.add id data s.all_players })
    (state.players |> Player_map.bindings |> shuffle)
    { state with round; all_players = Player_map.empty }
;;

type round_result =
  | Round_won of (Player.Id.t * player_data)
  | Draw

let run game_ref =
  let rec run_round tick =
    let game = !game_ref in
    if round_over game.state
    then (
      match round_winner game.state with
      | Some winner -> Round_won winner
      | None -> Draw)
    else (
      game_ref := { !game_ref with state = step game tick };
      Thread.delay 0.01;
      run_round (tick + 1))
  in
  let rec go round =
    let game = !game_ref in
    if round > game.settings.rounds
    then (
      print_endline "GAME OVER";
      let state = game.state in
      state.stats
      |> Player_map.iter (fun id { wins } ->
        let p = State.get_player id state in
        let module M = (val p.impl : PLAYER) in
        Printf.printf "  %s: %i wins\n%!" M.meta.name wins)
      (* TODO: announce (possibly multiple) winner(s) *))
    else (
      Printf.printf "Round %i starting...\n%!" round;
      game_ref := { game with state = init_round round game.settings game.state };
      match run_round 0 with
      | Round_won (id, winner) ->
        let game = !game_ref in
        game_ref
        := { game with
             state =
               { game.state with
                 stats =
                   Player_map.update
                     id
                     (function
                       | Some s -> Some { wins = s.wins + 1 }
                       | None -> failwith "stat update impossible")
                     game.state.stats
               }
           };
        let module M = (val winner.impl : PLAYER) in
        Printf.printf "Round %i won by '%s'!\n%!" round M.meta.name;
        Thread.delay 2.;
        go (round + 1)
      | Draw ->
        Printf.printf "Round %i ended in a draw!\n%!" round;
        go (round + 1))
  in
  go 1
;;
