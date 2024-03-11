let failwithf f = Printf.ksprintf failwith f
let arena_width = 1000
let arena_height = 800
let player_diameter = 50.

type player_command =
  | Move of float
  | Turn_right of float
[@@deriving show]

module type PLAYER = sig
  val on_tick : int -> player_command list
end

let lua_call_on_tick_event ls tick =
  Lua.getfield ls 1 "on_tick";
  if Lua.isnil ls (-1)
  then (
    Lua.pop ls 1;
    [])
  else (
    Lua.pushinteger ls tick;
    Lua.call ls 1 1;
    if Lua.istable ls (-1)
    then (
      (* TODO: read all the commands *)
      let _size = Lua.objlen ls (-1) in
      Lua.pushinteger ls 1;
      Lua.gettable ls (-2);
      match Lua.touserdata ls (-1) with
      | Some (`Userdata cmd) ->
        let commands = [ cmd ] in
        commands
      | _ -> [])
    else [])
;;

let make_lua_player ls : (module PLAYER) =
  (module struct
    let on_tick tick = lua_call_on_tick_event ls tick
  end)
;;

type position =
  { x : float
  ; y : float
  }

type player_intent =
  { distance : float
  ; angle : float
  }

let default_intent = { distance = 0.0; angle = 0.0 }

type player_state =
  { pos : position
  ; intent : player_intent
  }

let make_initial_state pos = { pos; intent = default_intent }

module Player_id = struct
  type t = int

  let compare (p1 : t) p2 = compare p1 p2
end

module PlayerMap = Map.Make (Player_id)

type player_meta =
  { name : string
  ; color : Color.t
  }

type player_data =
  { state : player_state ref
  ; impl : (module PLAYER)
  ; meta : player_meta
  }

module Game_state = struct
  type t = { players : player_data PlayerMap.t }

  let initial = { players = PlayerMap.empty }

  let add_player meta state impl game_state =
    let data = { meta; state; impl } in
    let new_id = 1 + (PlayerMap.bindings game_state.players |> List.length) in
    { players = PlayerMap.add new_id data game_state.players }
  ;;
end

let lua_get_color ls =
  Lua.getfield ls (-1) "color";
  Lua.getfield ls (-1) "red";
  let red = Lua.tointeger ls (-1) in
  Lua.getfield ls (-2) "green";
  let green = Lua.tointeger ls (-1) in
  Lua.getfield ls (-3) "blue";
  let blue = Lua.tointeger ls (-1) in
  Lua.pop ls 4;
  Color.make ~red ~green ~blue
;;

let lua_load_lua_player path =
  let ls = Lua.newstate () in
  Lua.openlibs ls;
  (* TODO: clean error handling *)
  if Lua.dofile ls path
  then (
    Printf.printf "loading player from '%s'...\n%!" path;
    Lua.getfield ls (-1) "meta";
    Lua.getfield ls (-1) "name";
    match Lua.tostring ls (-1) with
    | None -> failwithf "player.meta.name missing\n%!"
    | Some name ->
      (* pop the 'name' *)
      Lua.pop ls 1;
      let color = lua_get_color ls in
      (* pop the 'meta' table *)
      Lua.pop ls 1;
      let meta = { name; color } in
      meta, ls)
  else failwithf "player could not be loaded: '%s'\n%!" path
;;

let create_lua_api ls get_player_state =
  Lua.pushmodule
    ls
    "me"
    [ ( "x"
      , fun l ->
          Lua.pushnumber l (get_player_state ()).pos.x;
          1 )
    ; ( "y"
      , fun l ->
          Lua.pushnumber l (get_player_state ()).pos.y;
          1 )
    ; ( "move"
      , fun l ->
          let distance = Lua.tonumber l (-1) in
          Lua.pop ls 1;
          Lua.newuserdata ls (Move distance);
          1 )
    ]
;;

let load_lua_player path get_player_state =
  let player, lua_state = lua_load_lua_player ("players/" ^ path) in
  create_lua_api lua_state get_player_state;
  let p = make_lua_player lua_state in
  player, p
;;

let draw_player renderer player player_state =
  let x = player_state.pos.x -. (player_diameter /. 2.) |> Int.of_float in
  let y = player_state.pos.y -. (player_diameter /. 2.) |> Int.of_float in
  let rect = Sdl.Rect.create ~x ~y ~w:50 ~h:50 in
  let { color; _ } = player in
  Sdl.set_render_draw_color renderer ~r:color.red ~g:color.green ~b:color.blue;
  Sdl.render_fill_rect renderer rect
;;

(* TODO: implement: take the 'latest' (according to event order?) command of
   each type *)
let reduce_commands commands = commands

type movement_change =
  { remaining_intent : player_intent
  ; target_position : position
  }

let calculate_new_pos old_pos intent =
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
    | Move dist -> { intent with distance = dist }
    | _ -> intent (* TODO: direction + angle *)
  in
  List.fold_left apply_cmd old_intent cmds
;;

let dist p1 p2 = sqrt (Float.pow (p1.x -. p2.x) 2. +. Float.pow (p1.y -. p2.y) 2.)
let players_collide pos1 pos2 = dist pos1 pos2 <= player_diameter

(* TODO: only pass "obstacles" instead of whole state *)
let is_valid_position player_id ({ x; y } as p) (game_state : Game_state.t) =
  let r = player_diameter /. 2. in
  let stays_inside_arena =
    x -. r >= 0.
    && x +. r <= Float.of_int arena_width
    && y -. r >= 0.
    && y +. r <= Float.of_int arena_height
  in
  let would_hit_other_player =
    game_state.players
    |> PlayerMap.exists (fun id { state; _ } ->
      player_id != id && players_collide p !state.pos)
  in
  stays_inside_arena && not would_hit_other_player
;;

let find_colliding_players positions =
  let rec go (_player, movement_change) to_check acc =
    (* TODO: find out who collided with whom -> event *)
    let colliding =
      PlayerMap.filter
        (fun _p m -> players_collide movement_change.target_position m.target_position)
        to_check
    in
    let new_acc = List.append acc (PlayerMap.bindings colliding) in
    let remaining =
      (* TODO: map difference utility *)
      PlayerMap.merge
        (fun _player l r ->
          match l, r with
          | Some x, None -> Some x
          | _ -> None)
        to_check
        colliding
    in
    if PlayerMap.is_empty remaining
    then new_acc
    else (
      let next = PlayerMap.choose remaining in
      go next (PlayerMap.remove (fst next) remaining) new_acc)
  in
  match PlayerMap.choose_opt positions with
  | Some first ->
    let others = PlayerMap.remove (fst first) positions in
    go first others []
  | None -> []
;;

let run_tick game_state tick =
  let moving_players =
    game_state.players
    |> PlayerMap.filter_map (fun _id { state; impl; meta } ->
      Printf.printf "  asking player: %s\n%!" meta.name;
      let ps = !state in
      let module M = (val impl : PLAYER) in
      let tick_commands = M.on_tick tick in
      let commands = reduce_commands tick_commands in
      let new_intent = determine_intent ps.intent commands in
      (* TODO: always update intent? *)
      let desired_movement = calculate_new_pos ps.pos new_intent in
      desired_movement |> Option.map (fun m -> state, m))
    |> PlayerMap.filter (fun id (_state, movement_change) ->
      is_valid_position id movement_change.target_position game_state)
  in
  let collisions =
    moving_players |> PlayerMap.mapi (fun _ (_, m) -> m) |> find_colliding_players
  in
  moving_players
  |> PlayerMap.filter (fun id _ -> not @@ List.exists (fun (p, _) -> p == id) collisions)
  |> PlayerMap.iter (fun _id (state, movement_change) ->
    state
    := { pos = movement_change.target_position
       ; intent = movement_change.remaining_intent
       })
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
    |> PlayerMap.iter (fun _id { state; meta; _ } -> draw_player renderer meta !state);
    Sdl.render_present renderer;
    Thread.delay 0.01
  done
;;

let main () =
  let state1 = ref @@ make_initial_state { x = 100.; y = 50. } in
  let meta1, impl1 = load_lua_player "lloyd.lua" (fun () -> !state1) in
  let state2 = ref @@ make_initial_state { x = 450.; y = 80. } in
  let meta2, impl2 = load_lua_player "cole.lua" (fun () -> !state2) in
  let game_state =
    Game_state.initial
    |> Game_state.add_player meta1 state1 impl1
    |> Game_state.add_player meta2 state2 impl2
  in
  Sdl.with_sdl (fun () ->
    Sdl.with_window_and_renderer
      ~w:arena_width
      ~h:arena_height
      "Arena"
      (fun _window renderer -> main_loop renderer game_state))
;;
