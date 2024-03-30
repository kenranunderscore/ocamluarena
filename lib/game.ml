let failwithf f = Printf.ksprintf failwith f
let to_radians deg = deg *. Float.pi /. 180.

(* TODO: settings? configurable? *)
let arena_width = 1000.
let arena_height = 800.
let player_diameter = 50.
let player_radius = player_diameter /. 2.
let player_angle_of_vision = 0.9 *. Float.pi /. 2.
let max_turn_rate = to_radians 5.
let max_view_turn_rate = to_radians 10.

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

  let values m = bindings m |> List.map snd
end

type intent =
  { distance : float
  ; turn_angle : float
  ; view_angle : float
  ; attack : float option
  }

let default_intent = { distance = 0.0; turn_angle = 0.0; view_angle = 0.0; attack = None }

type player_state =
  { pos : Point.t
  ; heading : float
  ; view_direction : float
  ; intent : intent
  ; hp : int
  }

let is_dead player_state = player_state.hp <= 0

type player_data =
  { state : player_state
  ; impl : (module PLAYER)
  }

type attack_state =
  { pos : Point.t
  ; heading : float
  ; origin : Point.t
  ; owner : Player.Id.t
  }

module State = struct
  type t =
    { living_players : player_data Player_map.t
    ; dead_players : player_data Player_map.t
    ; attacks : attack_state list Player_map.t
    }

  let initial =
    { living_players = Player_map.empty
    ; dead_players = Player_map.empty
    ; attacks = Player_map.empty
    }
  ;;

  let get_player id state =
    match Player_map.find_opt id state.living_players with
    | Some player -> player
    | None -> Player_map.find id state.dead_players
  ;;
end

let players_collide (pos1 : Point.t) pos2 = Point.dist pos1 pos2 <= player_diameter

let inside_arena { Point.x; y } =
  x >= 0. && x <= arena_width && y >= 0. && y <= arena_height
;;

let rec first_with f pred =
  let res = f () in
  if pred res then res else first_with f pred
;;

let random_initial_state (state : State.t) =
  let random_coord dim =
    dim -. (2. *. player_diameter) |> Random.float |> ( +. ) player_diameter
  in
  let random_pos () =
    Point.make ~x:(random_coord arena_width) ~y:(random_coord arena_height)
  in
  let is_valid p =
    not
      (Player_map.exists
         (fun _id { state; _ } -> Point.dist p state.pos < 2. *. player_diameter)
         state.living_players)
  in
  let pos = first_with random_pos is_valid in
  let heading = Random.float (2. *. Float.pi) in
  let view_direction = Random.float (2. *. Float.pi) in
  { pos; heading; view_direction; hp = 100; intent = default_intent }
;;

let make_state_reader (id : Player.Id.t) (state : State.t ref) () =
  let player = State.get_player id !state in
  let { pos; heading; hp; view_direction; _ } = player.state in
  { Player.pos; heading; hp; view_direction }
;;

let add_player player_file (state_ref : State.t ref) =
  (* NOTE: assumes no players are dead *)
  let gs = !state_ref in
  let new_id =
    1 + (Player_map.bindings gs.living_players |> List.length) |> Player.Id.make
  in
  let player_state = random_initial_state gs in
  let impl = Player.Lua.load player_file (make_state_reader new_id state_ref) in
  let data = { state = player_state; impl } in
  state_ref := { gs with living_players = Player_map.add new_id data gs.living_players };
  state_ref
;;

let shuffle xs =
  xs |> List.map (fun x -> Random.bits (), x) |> List.sort compare |> List.map snd
;;

let start_new player_files seed =
  Random.init seed;
  (* Printf.printf "started new game with random seed %i\n%!" seed; *)
  let shuffled = shuffle player_files in
  shuffled |> List.iter print_endline;
  let state = ref State.initial in
  List.fold_right add_player (shuffle player_files) state
;;

let round_over (state : State.t) = Player_map.cardinal state.living_players <= 1
let round_winner (state : State.t) = Player_map.choose_opt state.living_players

(* TODO: implement: take the 'latest' (according to event order?) command of
   each type *)
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

let sign x = if Float.sign_bit x then -1. else 1.

(* TODO: inline? *)

let circles_intersect (p1 : Point.t) r1 (p2 : Point.t) r2 = Point.dist p1 p2 <= r1 +. r2

(* TODO: pull out common movement logic *)

let calculate_new_pos (p : Point.t) heading velocity =
  let dx = sin heading *. velocity in
  let dy = -.(cos heading *. velocity) in
  let x, y = p.x +. dx, p.y +. dy in
  Point.make ~x ~y
;;

let calculate_movement (p : Point.t) old_heading intent =
  let max_velocity = 1. in
  let velocity = Float.min intent.distance max_velocity in
  let dangle =
    sign intent.turn_angle *. Float.min max_turn_rate (Float.abs intent.turn_angle)
  in
  let turn_angle =
    if Float.abs intent.turn_angle < max_turn_rate
    then 0.
    else intent.turn_angle +. dangle
  in
  let heading = old_heading +. dangle in
  let dx = sin heading *. velocity in
  let dy = -.(cos heading *. velocity) in
  let distance = Float.max 0. (intent.distance -. velocity) in
  let x, y = p.x +. dx, p.y +. dy in
  let position = Point.make ~x ~y in
  let remaining_intent = { intent with distance; turn_angle } in
  { intent = remaining_intent; position; heading }
;;

let determine_intent old_intent cmds =
  let apply_cmd intent = function
    | Player.Move distance -> { intent with distance }
    | Turn_right turn_angle -> { intent with turn_angle }
    | Attack heading -> { intent with attack = Some heading }
    | Look_right view_angle -> { intent with view_angle }
  in
  List.fold_left apply_cmd old_intent cmds
;;

(* TODO: only pass "obstacles" instead of whole state *)
let is_valid_position
  (player_id : Player.Id.t)
  ({ x; y } as p : Point.t)
  (state : State.t)
  =
  let r = player_diameter /. 2. in
  let stays_inside_arena =
    x -. r >= 0. && x +. r <= arena_width && y -. r >= 0. && y +. r <= arena_height
  in
  let would_hit_other_player =
    state.living_players
    |> Player_map.exists (fun id { state; _ } ->
      player_id <> id && players_collide p state.pos)
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
   - transition: state * current intents -> new state * events
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

let event_index = function
  | Tick _ -> 0
  | Enemy_seen _ -> 1
  | Attack_hit _ -> 2
  | Hit_by _ -> 3
  | Death -> 4
;;

type targeted_event =
  | Global_event of event
  | Player_event of Player.Id.t * event

(* TODO: decide:
   - should this return vision events, or should these be decided AFTER movement?
   - or should this just be called after movement _and_ return vision events?
*)
let move_heads (state : State.t) =
  let living_players =
    state.living_players
    |> Player_map.map (fun p ->
      let intent = p.state.intent in
      let abs_angle = Float.abs intent.view_angle in
      let dangle = sign intent.view_angle *. Float.min max_view_turn_rate abs_angle in
      let angle =
        if abs_angle < max_view_turn_rate then 0. else intent.view_angle +. dangle
      in
      let intent = { intent with view_angle = angle } in
      (* TODO: modify/set_FOO functions on state *)
      let view_direction = Math.normalize_angle (p.state.view_direction +. dangle) in
      { p with state = { p.state with intent; view_direction } })
  in
  { state with living_players }, []
;;

(* TODO: move attacks or players first? *)
let move_players state =
  let player_moves =
    state.living_players
    |> Player_map.map (fun p ->
      let move = calculate_movement p.state.pos p.state.heading p.state.intent in
      p, move)
    |> Player_map.filter (fun id (_p, move) -> is_valid_position id move.position state)
  in
  let collisions = player_moves |> Player_map.map snd |> find_colliding_players in
  let moving_players =
    player_moves
    |> Player_map.filter (fun id _ ->
      not @@ List.exists (fun (id', _) -> id' = id) collisions)
    |> Player_map.map (fun (p, move) ->
      { p with
        state =
          { p.state with
            pos = move.position
          ; heading = move.heading
          ; intent = move.intent
          }
      })
  in
  let living_players =
    Player_map.union (fun _id _ b -> Some b) state.living_players moving_players
  in
  (* TODO: return events from movement execution (collisions etc.) *)
  { state with living_players }, []
;;

let players_hit attack players =
  players
  |> Player_map.filter (fun id player ->
    id <> attack.owner
    && circles_intersect player.state.pos player_radius attack.pos attack_radius)
;;

let transition_attacks (state : State.t) =
  let attacks_or_events =
    state.attacks
    |> Player_map.map (fun attacks ->
      List.fold_right
        (fun attack ((atts, evts) as acc) ->
          let velocity = 3. in
          let pos = calculate_new_pos attack.pos attack.heading velocity in
          if inside_arena pos
          then (
            let hits = players_hit attack state.living_players in
            if Player_map.is_empty hits
            then { attack with pos } :: atts, evts
            else
              ( atts
              , Player_map.mapi
                  (fun victim_id p ->
                    let owner = Player_map.find attack.owner state.living_players in
                    let module Victim = (val p.impl : PLAYER) in
                    let module Attacker = (val owner.impl : PLAYER) in
                    [ Player_event
                        (attack.owner, Attack_hit (Victim.meta.name, p.state.pos))
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
  let new_attacks =
    state.living_players
    |> Player_map.filter_map (fun id p ->
      Option.map
        (fun heading -> { origin = p.state.pos; owner = id; pos = p.state.pos; heading })
        p.state.intent.attack)
  in
  (* TODO: add "attacking player" events *)
  (* TODO: add state function to add attacks *)
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
  let living_players =
    state.living_players
    |> Player_map.map (fun p ->
      let intent = { p.state.intent with attack = None } in
      { p with state = { p.state with intent } })
  in
  { state with attacks; living_players }, []
;;

let apply_intents state =
  (* TODO: refactor *)
  let state, vision_events = move_heads state in
  let state, movement_events = move_players state in
  let state, attack_events = transition_attacks state in
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
    | Tick tick -> M.on_tick tick
    | Enemy_seen (name, pos) -> M.on_enemy_seen name pos
    | Attack_hit (name, pos) -> M.on_attack_hit name pos
    | Hit_by name -> M.on_hit_by name
    | Death ->
      M.on_death ();
      [])
;;

let update_intent player_data commands =
  let state = player_data.state in
  let new_intent = determine_intent state.intent commands in
  { player_data with state = { state with intent = new_intent } }
;;

let sort_events events =
  let comp e1 e2 = compare (event_index e1) (event_index e2) in
  events |> List.sort comp
;;

let distribute_events tick events (state : State.t) =
  let all_events = Global_event (Tick tick) :: events in
  let living_players =
    state.living_players
    |> Player_map.mapi (fun id p ->
      all_events
      |> List.filter_map (function
        | Global_event evt -> Some evt
        | Player_event (id', evt) when id' = id -> Some evt
        | Player_event _ -> None)
      |> sort_events
      |> events_to_commands p.impl
      |> reduce_commands
      |> update_intent p)
  in
  { state with living_players }
;;

type step_result =
  | Round_won of (Player.Id.t * player_data)
  | Draw
  | Next_state of State.t

let transition_hitpoints events state =
  let state =
    List.fold_right
      (fun evt (acc : State.t) ->
        match evt with
        | Global_event _ -> acc
        | Player_event (id, Hit_by _) ->
          let living_players =
            acc.living_players
            |> Player_map.update id (function
              | Some p ->
                let hp = p.state.hp - 20 in
                let state = { p.state with hp } in
                Some { p with state }
              | None -> failwith "impossible; player vanished")
          in
          { acc with living_players }
        | Player_event _ -> acc)
      events
      state
  in
  (* TODO: get state and events as single step *)
  let death_events =
    state.living_players
    |> Player_map.filter_map (fun id p ->
      if is_dead p.state then Some (Player_event (id, Death)) else None)
    |> Player_map.values
  in
  state, death_events
;;

let distribute_death_events events (state : State.t) =
  List.fold_right
    (fun evt acc ->
      match evt with
      | Player_event (id, Death) ->
        let player = state.living_players |> Player_map.find id in
        { state with
          living_players = Player_map.remove id state.living_players
        ; dead_players = Player_map.add id player state.dead_players
        }
      | _ -> acc)
    events
    state
;;

let can_spot player other_player =
  let p = player.state.pos in
  let q = other_player.state.pos in
  (* relative: -pi to pi *)
  let angle = Math.angle_between p q in
  (* absolute 0 to pi *)
  let view_direction = player.state.view_direction in
  let dangle = player_angle_of_vision /. 2. in
  let left = view_direction -. dangle in
  let right = view_direction +. dangle in
  (* TODO: accommodate for player size/radius *)
  (* TODO: simplify *)
  Math.is_between angle left right
  || Math.is_between (angle +. (2. *. Float.pi)) left right
  || Math.is_between (angle -. (2. *. Float.pi)) left right
;;

let vision_events (state : State.t) =
  state.living_players
  |> Player_map.mapi (fun id p ->
    state.living_players
    |> Player_map.filter_map (fun id' p' ->
      if id <> id' && can_spot p p'
      then
        let module M = (val p'.impl : PLAYER) in
        Some (Player_event (id, Enemy_seen (M.meta.name, p'.state.pos)))
      else None)
    |> Player_map.values)
  |> Player_map.values
  |> List.concat
;;

let step (state : State.t) tick =
  (* smells like state monad *)
  let enemy_seen_events = vision_events state in
  let state, intent_events = apply_intents state in
  let state, death_events = transition_hitpoints intent_events state in
  let state = distribute_death_events death_events state in
  let all_events = List.concat [ intent_events; death_events; enemy_seen_events ] in
  distribute_events tick all_events state
;;
