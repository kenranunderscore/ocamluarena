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

module PlayerStates = Map.Make (Player)

type game_state = { player_states : player_state ref PlayerStates.t }

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
    | "cole.lua" -> { x = 400; y = 450 }
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

let calculate_new_pos old_pos intent =
  (* TODO: fix direction *)
  let delta = if Float.sign_bit intent.distance then -1.0 else 1.0 in
  if Float.abs intent.distance > 0.0
  then
    Some
      ( { old_pos with x = old_pos.x + Int.of_float delta }
      , { intent with distance = Float.sub intent.distance delta } )
  else None
;;

let determine_intent old_intent cmds =
  match cmds with
  | [ Move dist ] -> { old_intent with distance = dist }
  (* TODO: implement *)
  | _ -> old_intent
;;

let is_valid_position { x; y } _game_state =
  let r = player_diameter / 2 in
  x - r >= 0 && x + r <= arena_width && y - r >= 0 && y + r <= arena_height
;;

let run_tick game_state tick =
  let ps =
    game_state.player_states
    |> PlayerStates.mapi (fun player state ->
      Printf.printf "  asking player: %s\n%!" player.name;
      let ps = !state in
      let ls = ps.lua_state in
      let tick_commands = call_on_tick_event ls tick in
      let commands = reduce_commands tick_commands in
      let new_intent = determine_intent ps.intent commands in
      let desired_movement = calculate_new_pos ps.pos new_intent in
      state, desired_movement)
  in
  ps
  |> PlayerStates.mapi (fun _player (state, movement) ->
    match movement with
    | None -> state, None
    | Some (new_pos, _remaining_intent) ->
      if is_valid_position new_pos game_state
      then state, movement
      else state, None (* TODO *))
  |> PlayerStates.iter (fun _player (state, movement) ->
    match movement with
    | None -> ()
    | Some (new_pos, remaining_intent) ->
      if is_valid_position new_pos game_state
      then (
        print_endline "in here";
        state := { !state with pos = new_pos; intent = remaining_intent })
      else ())
;;

(* TODO: second collision checking phase goes here *)

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
    |> PlayerStates.iter (fun p state -> draw_player renderer p !state);
    Sdl.render_present renderer;
    Thread.delay 0.1
  done
;;

let main () =
  let players = [ load_player "lloyd.lua"; load_player "cole.lua" ] in
  let game_state = { player_states = PlayerStates.of_list players } in
  Sdl.with_sdl (fun () ->
    Sdl.with_window_and_renderer
      ~w:arena_width
      ~h:arena_height
      "Arena"
      (fun _window renderer -> main_loop renderer game_state))
;;
