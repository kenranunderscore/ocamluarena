let failwithf f = Printf.ksprintf failwith f

type meta =
  { name : string
  ; color : Color.t
  }

type movement_direction =
  | Forward
  | Backward
  | Left
  | Right
[@@deriving show]

type command =
  | Move of movement_direction * float
  | Turn_right of float
  | Attack of float
  | Look_right of float
[@@deriving show]

let command_index = function
  | Move _ -> 0
  | Turn_right _ -> 1
  | Attack _ -> 2
  | Look_right _ -> 3
;;

module Id = struct
  include Int

  let make n = n
end

type impl =
  { meta : meta
  ; on_round_started : int -> command list
  ; on_tick : int -> command list
  ; on_enemy_seen : string -> Point.t -> command list
  ; on_attack_hit : string -> Point.t -> command list
  ; on_hit_by : string -> command list
  ; on_death : unit -> unit
  ; on_round_over : string option -> unit
  ; on_round_won : unit -> unit
  }

type player_info =
  { hp : int
  ; pos : Point.t
  ; heading : float
  ; view_direction : float
  }

module Lua = struct
  (** Read all the commands returned as the result of calling an event handler.
      This expects the returned table to be on the top of the stack. *)
  let lua_read_commands ls =
    if Lua.istable ls (-1)
    then (
      match Lua.objlen ls (-1) with
      | 0 -> []
      | size ->
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

  let lua_tick ls tick =
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

  let lua_round_started ls round =
    Lua.getfield ls 1 "on_round_started";
    if Lua.isnil ls (-1)
    then (
      Lua.pop ls 1;
      [])
    else (
      Lua.pushinteger ls round;
      Lua.call ls 1 1;
      lua_read_commands ls)
  ;;

  let lua_enemy_seen ls name (pos : Point.t) =
    Lua.getfield ls 1 "on_enemy_seen";
    if Lua.isnil ls (-1)
    then (
      Lua.pop ls 1;
      [])
    else (
      (* TODO: custom payload -> lua table? *)
      Lua.pushstring ls name;
      Lua.pushnumber ls pos.x;
      Lua.pushnumber ls pos.y;
      Lua.call ls 3 1;
      lua_read_commands ls)
  ;;

  let lua_attack_hit ls name (pos : Point.t) =
    Lua.getfield ls 1 "on_attack_hit";
    if Lua.isnil ls (-1)
    then (
      Lua.pop ls 1;
      [])
    else (
      (* TODO: custom payload -> lua table? *)
      Lua.pushstring ls name;
      Lua.pushnumber ls pos.x;
      Lua.pushnumber ls pos.y;
      Lua.call ls 3 1;
      lua_read_commands ls)
  ;;

  let lua_hit_by ls name =
    Lua.getfield ls 1 "on_hit_by";
    if Lua.isnil ls (-1)
    then (
      Lua.pop ls 1;
      [])
    else (
      Lua.pushstring ls name;
      Lua.call ls 1 1;
      lua_read_commands ls)
  ;;

  let lua_on_death ls =
    Lua.getfield ls 1 "on_death";
    if Lua.isnil ls (-1) then Lua.pop ls 1 else Lua.call ls 0 0
  ;;

  let lua_on_round_over ls mname =
    Lua.getfield ls 1 "on_round_over";
    if Lua.isnil ls (-1)
    then Lua.pop ls 1
    else (
      (match mname with
       | Some name -> Lua.pushstring ls name
       | None -> Lua.pushnil ls);
      Lua.call ls 1 0)
  ;;

  let lua_on_round_won ls =
    Lua.getfield ls 1 "on_round_won";
    if Lua.isnil ls (-1) then Lua.pop ls 1 else Lua.call ls 0 0
  ;;

  let make_lua_player ls meta =
    { meta
    ; on_round_started = (fun round -> lua_round_started ls round)
    ; on_tick = (fun tick -> lua_tick ls tick)
    ; on_enemy_seen = (fun name pos -> lua_enemy_seen ls name pos)
    ; on_attack_hit = (fun name pos -> lua_attack_hit ls name pos)
    ; on_hit_by = (fun name -> lua_hit_by ls name)
    ; on_death = (fun () -> lua_on_death ls)
    ; on_round_over = (fun mname -> lua_on_round_over ls mname)
    ; on_round_won = (fun () -> lua_on_round_won ls)
    }
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

  let create_lua_api ls meta get_player_info =
    Lua.pushmodule
      ls
      "me"
      [ ( "x"
        , fun l ->
            Lua.pushnumber l (get_player_info ()).pos.x;
            1 )
      ; ( "y"
        , fun l ->
            Lua.pushnumber l (get_player_info ()).pos.y;
            1 )
      ; ( "position"
        , fun l ->
            let p = (get_player_info ()).pos in
            Lua.newtable l;
            Lua.pushnumber l p.x;
            Lua.setfield l (-2) "x";
            Lua.pushnumber l p.y;
            Lua.setfield l (-2) "y";
            1 )
      ; ( "heading"
        , fun l ->
            Lua.pushnumber l (get_player_info ()).heading;
            1 )
      ; ( "view_direction"
        , fun l ->
            Lua.pushnumber l (get_player_info ()).view_direction;
            1 )
      ; ( "hp"
        , fun l ->
            Lua.pushinteger l (get_player_info ()).hp;
            1 )
      ; ( "move_forward"
        , fun l ->
            let distance = Lua.tonumber l (-1) in
            Lua.pop l 1;
            Lua.newuserdata l (Move (Forward, distance));
            1 )
      ; ( "move_backward"
        , fun l ->
            let distance = Lua.tonumber l (-1) in
            Lua.pop l 1;
            Lua.newuserdata l (Move (Backward, distance));
            1 )
      ; ( "move_left"
        , fun l ->
            let distance = Lua.tonumber l (-1) in
            Lua.pop l 1;
            Lua.newuserdata l (Move (Left, distance));
            1 )
      ; ( "move_right"
        , fun l ->
            let distance = Lua.tonumber l (-1) in
            Lua.pop l 1;
            Lua.newuserdata l (Move (Right, distance));
            1 )
      ; ( "attack"
        , fun l ->
            let heading = Lua.tonumber l (-1) in
            Lua.pop l 1;
            Lua.newuserdata l (Attack heading);
            1 )
      ; ( "turn_right"
        , fun l ->
            let angle = Lua.tonumber l (-1) in
            Lua.pop l 1;
            Lua.newuserdata l (Turn_right angle);
            1 )
      ; ( "turn_left"
        , fun l ->
            let angle = Lua.tonumber l (-1) in
            Lua.pop l 1;
            Lua.newuserdata l (Turn_right (-.angle));
            1 )
      ; ( "look_right"
        , fun l ->
            let angle = Lua.tonumber l (-1) in
            Lua.pop l 1;
            Lua.newuserdata l (Look_right angle);
            1 )
      ; ( "look_left"
        , fun l ->
            let angle = Lua.tonumber l (-1) in
            Lua.pop l 1;
            Lua.newuserdata l (Look_right (-.angle));
            1 )
      ; ( "log"
        , fun l ->
            (match Lua.tostring l (-1) with
             | Some msg -> Printf.printf "[%s] %s\n%!" meta.name msg
             | None -> ());
            Lua.pop l 1;
            0 )
      ]
  ;;

  let load path get_player_info =
    let meta, lua_state = lua_load_player_from_file ("players/" ^ path) in
    create_lua_api lua_state meta get_player_info;
    make_lua_player lua_state meta
  ;;
end
