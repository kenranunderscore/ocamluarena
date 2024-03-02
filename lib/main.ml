let failwithf f = Printf.ksprintf failwith f

type player =
  { name : string
  ; color : Color.t
  }

type position =
  { x : int
  ; y : int
  }

type player_state =
  { player : player
  ; pos : position
  ; lua_state : Lua.state
  }

type game_state =
  { player1 : player_state ref
  ; player2 : player_state ref
  }

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
      let player = { name; color } in
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
  let player_state = ref { player; pos; lua_state } in
  create_lua_api player_state;
  player_state
;;

let draw_player renderer player =
  let rect = Sdl.Rect.create ~x:player.pos.x ~y:player.pos.y ~w:50 ~h:50 in
  let color = player.player.color in
  Sdl.set_render_draw_color renderer ~r:color.red ~g:color.green ~b:color.blue;
  Sdl.render_fill_rect renderer rect
;;

let run_tick game_state tick =
  [ !(game_state.player1); !(game_state.player2) ]
  |> List.iter (fun ps ->
    Printf.printf "  asking player: %s\n%!" ps.player.name;
    Lua.getfield ps.lua_state 1 "on_tick";
    if Lua.isnil ps.lua_state (-1)
    then Lua.pop ps.lua_state 1
    else (
      Lua.pushinteger ps.lua_state !tick;
      Lua.call ps.lua_state 1 0))
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
    run_tick game_state tick;
    Sdl.set_render_draw_color renderer ~r:20 ~g:20 ~b:20;
    Sdl.render_clear renderer;
    !(game_state.player1) |> draw_player renderer;
    !(game_state.player2) |> draw_player renderer;
    Sdl.render_present renderer;
    Thread.delay 1.0
  done
;;

let main () =
  let player1 = load_player "lloyd.lua" in
  let player2 = load_player "cole.lua" in
  let game_state = { player1; player2 } in
  Sdl.with_sdl (fun () ->
    Sdl.with_window_and_renderer ~w:1000 ~h:800 "Arena" (fun _window renderer ->
      main_loop renderer game_state))
;;
