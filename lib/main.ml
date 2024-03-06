let failwithf f = Printf.ksprintf failwith f
let arena_width = 1000
let arena_height = 800
let player_diameter = 50

module Player = struct
  type t =
    { name : string
    ; color : Color.t
    }

  let make ~name ~color = { name; color }
  let name { name; _ } = name
  let color { color; _ } = color
  let compare p1 p2 = compare p1.name p2.name
end

type position =
  { x : int
  ; y : int
  }

type player_command =
  | Move of float
  | Turn_right of float
[@@deriving show]

type player_intent =
  { distance : float
  ; angle : float
  }

let default_intent = { distance = 0.0; angle = 0.0 }

type player_state =
  { lua_state : Lua.state
  ; pos : position
  ; commands : player_command list
  ; intent : player_intent
  }

module PlayerMap = Map.Make (Player)

type game_state = { player_states : player_state ref PlayerMap.t }

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

(* TODO: maybe add some unique identifier *)
let lua_load_player path =
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
      let player = Player.make ~name ~color in
      player, ls)
  else failwithf "player could not be loaded: '%s'\n%!" path
;;

let create_lua_api player_state =
  let ls = !player_state.lua_state in
  Lua.pushmodule
    ls
    "me"
    [ ( "x"
      , fun l ->
          Lua.pushinteger l !player_state.pos.x;
          1 )
    ; ( "y"
      , fun l ->
          Lua.pushinteger l !player_state.pos.y;
          1 )
    ; ( "move"
      , fun l ->
          let distance = Lua.tonumber l (-1) in
          Lua.pop ls 1;
          Lua.newuserdata ls (Move distance);
          1 )
    ]
;;

let load_player path =
  let player, lua_state = lua_load_player ("players/" ^ path) in
  let pos =
    (* workaround *)
    match path with
    | "lloyd.lua" -> { x = 100; y = 50 }
    | "cole.lua" -> { x = 200; y = 50 }
    | _ -> { x = 0; y = 0 }
  in
  let player_state = ref { pos; lua_state; commands = []; intent = default_intent } in
  create_lua_api player_state;
  player, player_state
;;

let draw_player renderer player player_state =
  let x = player_state.pos.x - (player_diameter / 2) in
  let y = player_state.pos.y - (player_diameter / 2) in
  let rect = Sdl.Rect.create ~x ~y ~w:50 ~h:50 in
  let color = Player.color player in
  Sdl.set_render_draw_color renderer ~r:color.red ~g:color.green ~b:color.blue;
  Sdl.render_fill_rect renderer rect
;;

(* TODO: implement: take the 'latest' (according to event order?) command of
   each type *)
let reduce_commands commands = commands

let call_on_tick_event ls tick =
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
    let target_position = { old_pos with x = old_pos.x + Int.of_float delta } in
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

let is_valid_position { x; y } _game_state =
  (* also check game state wrt. player positions *)
  let r = player_diameter / 2 in
  x - r >= 0 && x + r <= arena_width && y - r >= 0 && y + r <= arena_height
;;

let dist p1 p2 =
  sqrt
    (Float.pow (Float.of_int p1.x -. Float.of_int p2.x) 2.
     +. Float.pow (Float.of_int p1.y -. Float.of_int p2.y) 2.)
;;

let players_collide pos1 pos2 = dist pos1 pos2 <= Float.of_int player_diameter

let find_colliding_players positions =
  let rec go (_player, movement_change) to_check acc =
    let colliding =
      PlayerMap.filter
        (fun _p m -> players_collide movement_change.target_position m.target_position)
        to_check
    in
    (* Printf.printf "  colliding.length == %i\n%!" (PlayerMap.cardinal colliding) *)
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
    game_state.player_states
    |> PlayerMap.filter_map (fun player state ->
      Printf.printf "  asking player: %s\n%!" player.name;
      let ps = !state in
      let ls = ps.lua_state in
      let tick_commands = call_on_tick_event ls tick in
      let commands = reduce_commands tick_commands in
      let new_intent = determine_intent ps.intent commands in
      (* TODO: always update intent? *)
      let desired_movement = calculate_new_pos ps.pos new_intent in
      desired_movement |> Option.map (fun m -> state, m))
    |> PlayerMap.filter (fun _player (_state, movement_change) ->
      is_valid_position movement_change.target_position game_state)
  in
  let collisions =
    moving_players |> PlayerMap.mapi (fun _ (_, m) -> m) |> find_colliding_players
  in
  (* TODO: second collision checking phase goes here *)
  moving_players
  |> PlayerMap.iter (fun _player (state, movement_change) ->
    state
    := { !state with
         pos = movement_change.target_position
       ; intent = movement_change.remaining_intent
       })
;;

let main_loop renderer game_state =
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
    game_state.player_states
    |> PlayerMap.iter (fun p state -> draw_player renderer p !state);
    Sdl.render_present renderer;
    Thread.delay 0.01
  done
;;

let main () =
  let players = [ load_player "lloyd.lua"; load_player "cole.lua" ] in
  let game_state = { player_states = PlayerMap.of_list players } in
  Sdl.with_sdl (fun () ->
    Sdl.with_window_and_renderer
      ~w:arena_width
      ~h:arena_height
      "Arena"
      (fun _window renderer -> main_loop renderer game_state))
;;
