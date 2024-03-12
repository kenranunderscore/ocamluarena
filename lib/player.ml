let failwithf f = Printf.ksprintf failwith f

type meta =
  { name : string
  ; color : Color.t
  }

type command =
  | Move of float
  | Turn_right of float
[@@deriving show]

module Id = struct
  include Int

  let make n = n
end

module type PLAYER = sig
  val meta : meta
  val on_tick : int -> command list
end

module Lua = struct
  (** Read all the commands returned as the result of calling an event handler.
      This expects the returned table to be on the top of the stack. *)
  let lua_read_commands ls =
    if Lua.istable ls (-1)
    then (
      match Lua.objlen ls (-1) with
      | 0 -> []
      | size ->
        Printf.printf "size: %i\n%!" size;
        let rec go index cmds =
          Lua.pushinteger ls index;
          Lua.gettable ls (-2);
          match Lua.touserdata ls (-1) with
          | Some (`Userdata cmd) ->
            Lua.pop ls 1;
            let new_acc = cmd :: cmds in
            if index = size then new_acc else go (index + 1) new_acc
          | Some _ -> failwith "lightuserdata"
          | None -> failwith "unexpected return value"
        in
        List.rev (go 1 []))
    else []
  ;;

  let lua_call_on_tick_event ls tick =
    Lua.getfield ls 1 "on_tick";
    if Lua.isnil ls (-1)
    then (
      Lua.pop ls 1;
      [])
    else (
      Lua.pushinteger ls tick;
      Lua.call ls 1 1;
      lua_read_commands ls)
  ;;

  let make_lua_player ls meta : (module PLAYER) =
    (module struct
      let meta = meta
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

  let create_lua_api ls (get_pos : unit -> Point.t * float) =
    Lua.pushmodule
      ls
      "me"
      [ ( "x"
        , fun l ->
            Lua.pushnumber l (get_pos () |> fst).x;
            1 )
      ; ( "y"
        , fun l ->
            Lua.pushnumber l (get_pos () |> fst).y;
            1 )
      ; ( "position"
        , fun l ->
            let p = get_pos () |> fst in
            Lua.newtable l;
            Lua.pushnumber l p.x;
            Lua.setfield l (-2) "x";
            Lua.pushnumber l p.y;
            Lua.setfield l (-2) "y";
            1 )
      ; ( "heading"
        , fun l ->
            Lua.pushnumber l (get_pos () |> snd);
            1 )
      ; ( "move"
        , fun l ->
            let distance = Lua.tonumber l (-1) in
            Lua.pop ls 1;
            Lua.newuserdata ls (Move distance);
            1 )
      ; ( "turn_right"
        , fun l ->
            let angle = Lua.tonumber l (-1) in
            Lua.pop ls 1;
            Lua.newuserdata ls (Turn_right angle);
            1 )
      ; ( "turn_left"
        , fun l ->
            let angle = Lua.tonumber l (-1) in
            Lua.pop ls 1;
            Lua.newuserdata ls (Turn_right (-.angle));
            1 )
      ]
  ;;

  let load path (get_pos : unit -> Point.t * float) =
    let meta, lua_state = lua_load_player_from_file ("players/" ^ path) in
    create_lua_api lua_state get_pos;
    make_lua_player lua_state meta
  ;;
end
