let failwithf f = Printf.ksprintf failwith f

type player =
  { name : string
  ; x : int
  ; y : int
  ; color : Color.t
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

(* add some unique identifier *)
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
      let player = { name; color; x = 100; y = 300 } in
      player, ls)
  else failwithf "player could not be loaded: '%s'\n%!" path
;;

type game_state =
  { player1 : player * Lua.state
  ; player2 : player * Lua.state
  }

let create_lua_api game_state =
  let state = !game_state in
  let _, ls1 = state.player1 in
  Lua.pushmodule
    ls1
    "me"
    [ ( "x"
      , fun l ->
          Lua.pushinteger l (!game_state.player1 |> fst).x;
          1 )
    ];
  let _, ls2 = state.player2 in
  Lua.pushmodule
    ls2
    "me"
    [ ( "x"
      , fun l ->
          Lua.pushinteger l (!game_state.player2 |> fst).x;
          1 )
    ]
;;

let draw_player renderer player =
  let rect = Sdl.Rect.create ~x:player.x ~y:player.y ~w:50 ~h:50 in
  let color = player.color in
  Sdl.set_render_draw_color renderer ~r:color.red ~g:color.green ~b:color.blue;
  Sdl.render_fill_rect renderer rect
;;

let main_loop renderer game_state =
  let e = Sdl.Event.create () in
  let quit = ref false in
  while not !quit do
    while Sdl.poll_event e && not !quit do
      match Sdl.Event.(enum (get e typ)) with
      | `Quit -> quit := true
      | `Key_up ->
        (match Sdl.Event.(get e keyboard_scancode) |> Sdl.Scancode.enum with
         | `Escape -> quit := true
         | `T ->
           let p, ls = !game_state.player2 in
           Printf.printf "player: %s\n%!" p.name;
           Lua.getfield ls 1 "test";
           Lua.call ls 0 0
         | _ -> ())
      | _ -> ()
    done;
    Sdl.set_render_draw_color renderer ~r:20 ~g:20 ~b:20;
    Sdl.render_clear renderer;
    !game_state.player1 |> fst |> draw_player renderer;
    !game_state.player2 |> fst |> draw_player renderer;
    Sdl.render_present renderer
  done
;;

let main () =
  let player1 = lua_load_player "players/lloyd.lua" in
  let player2 = lua_load_player "players/cole.lua" in
  let game_state = ref { player1; player2 } in
  create_lua_api game_state;
  Sdl.with_sdl (fun () ->
    Sdl.with_window_and_renderer ~w:1000 ~h:800 "Arena" (fun _window renderer ->
      main_loop renderer game_state))
;;
