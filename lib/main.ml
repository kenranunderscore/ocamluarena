module LuaL = Lua_api.LuaL
module Lua = Lua_api.Lua

let failwithf f = Printf.ksprintf failwith f

type color =
  { red : int
  ; green : int
  ; blue : int
  }

type player =
  { name : string
  ; x : int
  ; y : int
  ; color : color
  }

let lua_get_color ls =
  Lua.getfield ls (-1) "color";
  Lua.getfield ls (-1) "red";
  let red = Lua.tointeger ls (-1) in
  Lua.getfield ls (-2) "green";
  let green = Lua.tointeger ls (-1) in
  Lua.getfield ls (-3) "green";
  let blue = Lua.tointeger ls (-1) in
  Lua.pop ls 4;
  { red; green; blue }
;;

let lua_load_player path =
  let ls = LuaL.newstate () in
  LuaL.openlibs ls;
  (* TODO: clean error handling *)
  if LuaL.dofile ls path
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
       (player, ls)
  )
  else failwithf "player could not be loaded: '%s'\n%!" path
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
         | _ -> ())
      | _ -> ()
    done;
    Sdl.set_render_draw_color renderer ~r:20 ~g:20 ~b:20;
    Sdl.render_clear renderer;
    draw_player renderer !game_state;
    Sdl.render_present renderer
  done
;;

let main () =
  let player, _ls = lua_load_player "players/test.lua" in
  Sdl.with_sdl (fun () ->
    Sdl.with_window_and_renderer ~w:1000 ~h:800 "Arena" (fun _window renderer ->
      main_loop renderer (ref player)))
;;
