let failwithf f = Printf.ksprintf failwith f

type meta =
  { name : string
  ; color : Color.t
  }

type command =
  | Move of float
  | Turn_right of float
[@@deriving show]

type intent =
  { distance : float
  ; angle : float
  }

let default_intent = { distance = 0.0; angle = 0.0 }

type state =
  { pos : Point.t
  ; intent : intent
  }

let make_state ~pos ~intent = { pos; intent }
let make_initial_state pos = { pos; intent = default_intent }

module Id = struct
  type t = int

  let compare (p1 : t) p2 = compare p1 p2
end

module type PLAYER = sig
  val on_tick : int -> command list
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

let lua_load_player_from_file path =
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
  let player, lua_state = lua_load_player_from_file ("players/" ^ path) in
  create_lua_api lua_state get_player_state;
  let p = make_lua_player lua_state in
  player, p
;;
