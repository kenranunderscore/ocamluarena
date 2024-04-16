let failwithf f = Printf.ksprintf failwith f

type t =
  { name : string
  ; color : Color.t
  ; version : string
  ; entrypoint : string
  }

let compare p1 p2 = String.compare p1.name p2.name

type movement_direction =
  | Forward
  | Backward
  | Left
  | Right
[@@deriving show]

let string_of_direction = function
  | Forward -> "forward"
  | Backward -> "backward"
  | Left -> "left"
  | Right -> "right"
;;

module Command = struct
  type t =
    | Move of movement_direction * float
    | Attack
    | Turn of float
    | Turn_head of float
    | Turn_arms of float
  [@@deriving show]

  let index = function
    | Move _ -> 0
    | Attack -> 1
    | Turn _ -> 2
    | Turn_head _ -> 3
    | Turn_arms _ -> 4
  ;;

  let compare cmd1 cmd2 = Int.compare (index cmd1) (index cmd2)
end

module Id = struct
  type t = int

  let make n = n
  let compare = Int.compare
  let show = Int.to_string
end

(* TODO: replace strings with t's *)
type impl =
  { on_round_started : int -> Command.t list
  ; on_tick : int -> Command.t list
  ; on_enemy_seen : string -> Point.t -> Command.t list
  ; on_attack_hit : string -> Point.t -> Command.t list
  ; on_hit_by : string -> Command.t list
  ; on_death : unit -> unit
  ; on_round_over : string option -> unit
  ; on_round_won : unit -> unit
  }

type info =
  { hp : int
  ; pos : Point.t
  ; heading : float
  ; view_direction : float
  ; attack_direction : float
  }

module Lua = struct
  let read_direction ls =
    Lua.getfield ls (-1) "direction";
    let direction =
      match Lua.tostring ls (-1) with
      | Some "forward" -> Forward
      | Some "backward" -> Backward
      | Some "left" -> Left
      | Some "right" -> Right
      | Some other -> failwithf "invalid direction: %s" other
      | None -> failwith "direction not a string"
    in
    Lua.pop ls 1;
    direction
  ;;

  (** Read all the commands returned as the result of calling an event handler.
      This expects the returned table to be on the top of the stack. *)
  let lua_read_commands ls =
    let commands =
      if Lua.istable ls (-1)
      then (
        match Lua.objlen ls (-1) with
        | 0 -> []
        | size ->
          let rec go index cmds =
            Lua.pushinteger ls index;
            Lua.gettable ls (-2);
            Lua.getfield ls (-1) "tag";
            let cmd =
              match Lua.tostring ls (-1) with
              | Some "move" ->
                Lua.pop ls 1;
                let direction = read_direction ls in
                Lua.getfield ls (-1) "distance";
                let distance = Lua.tonumber ls (-1) in
                Lua.pop ls 1;
                Command.Move (direction, distance)
              | Some "attack" ->
                Lua.pop ls 1;
                Attack
              | Some "turn_right" ->
                Lua.pop ls 1;
                Lua.getfield ls (-1) "angle";
                let angle = Lua.tonumber ls (-1) in
                Lua.pop ls 1;
                Turn angle
              | Some "turn_head_right" ->
                Lua.pop ls 1;
                Lua.getfield ls (-1) "angle";
                let angle = Lua.tonumber ls (-1) in
                Lua.pop ls 1;
                Turn_head angle
              | Some "turn_arms_right" ->
                Lua.pop ls 1;
                Lua.getfield ls (-1) "angle";
                let angle = Lua.tonumber ls (-1) in
                Lua.pop ls 1;
                Turn_arms angle
              | Some s -> failwithf "unknown tag: %s" s
              | None -> failwith "no tag found"
            in
            (* the list *)
            Lua.pop ls 1;
            let new_acc = cmd :: cmds in
            if index = size then new_acc else go (index + 1) new_acc
          in
          List.rev (go 1 []))
      else []
    in
    Lua.pop ls 1;
    commands
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
    (* NOTE: this should alert early if any functions are unexpectedly
       unbalanced *)
    Lua.assert_stack_size ls 1;
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

  let make_lua_player ls =
    { on_round_started = (fun round -> lua_round_started ls round)
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
    Lua.getglobal ls "color";
    if Lua.istable ls (-1)
    then (
      Lua.getfield ls (-1) "red";
      let red = Lua.tointeger ls (-1) in
      Lua.getfield ls (-2) "green";
      let green = Lua.tointeger ls (-1) in
      Lua.getfield ls (-3) "blue";
      let blue = Lua.tointeger ls (-1) in
      Color.make ~red ~green ~blue |> Option.some)
    else None
  ;;

  let lua_load_player_from_file path =
    let ls = Lua.newstate () in
    Lua.openlibs ls;
    (* TODO: clean error handling *)
    if Lua.dofile ls path
    then (
      Printf.printf "loading player from '%s'...\n%!" path;
      ls)
    else failwithf "player could not be loaded: '%s'\n%!" path
  ;;

  module Api = struct
    let x get_player_info ls =
      Lua.pushnumber ls (get_player_info ()).pos.x;
      1
    ;;

    let y get_player_info ls =
      Lua.pushnumber ls (get_player_info ()).pos.y;
      1
    ;;

    let position get_player_info ls =
      let p = (get_player_info ()).pos in
      Lua.newtable ls;
      Lua.pushnumber ls p.x;
      Lua.setfield ls (-2) "x";
      Lua.pushnumber ls p.y;
      Lua.setfield ls (-2) "y";
      1
    ;;

    let heading get_player_info ls =
      Lua.pushnumber ls (get_player_info ()).heading;
      1
    ;;

    let view_direction get_player_info ls =
      Lua.pushnumber ls (get_player_info ()).view_direction;
      1
    ;;

    let attack_direction get_player_info ls =
      Lua.pushnumber ls (get_player_info ()).attack_direction;
      1
    ;;

    let hp get_player_info ls =
      Lua.pushinteger ls (get_player_info ()).hp;
      1
    ;;

    let push_tag ls tag =
      Lua.pushstring ls tag;
      Lua.setfield ls (-2) "tag"
    ;;

    let move direction ls =
      let distance = Lua.tonumber ls (-1) in
      Lua.pop ls 1;
      Lua.newtable ls;
      push_tag ls "move";
      Lua.pushstring ls (string_of_direction direction);
      Lua.setfield ls (-2) "direction";
      Lua.pushnumber ls distance;
      Lua.setfield ls (-2) "distance";
      1
    ;;

    let attack ls =
      Lua.newtable ls;
      push_tag ls "attack";
      1
    ;;

    let turn_body_part left tag ls =
      let angle = Lua.tonumber ls (-1) in
      Lua.pop ls 1;
      Lua.newtable ls;
      push_tag ls tag;
      Lua.pushnumber ls (if left then -.angle else angle);
      Lua.setfield ls (-2) "angle";
      1
    ;;

    let log name ls =
      (match Lua.tostring ls (-1) with
       | Some msg -> Printf.printf "[%s] %s\n%!" name msg
       | None -> ());
      Lua.pop ls 1;
      0
    ;;
  end

  let create_lua_api ls name get_player_info =
    Lua.pushmodule
      ls
      "me"
      [ "x", Api.x get_player_info
      ; "y", Api.y get_player_info
      ; "position", Api.position get_player_info
      ; "heading", Api.heading get_player_info
      ; "view_direction", Api.view_direction get_player_info
      ; "attack_direction", Api.attack_direction get_player_info
      ; "hp", Api.hp get_player_info
      ; "move", Api.move Forward
      ; "move_forward", Api.move Forward
      ; "move_backward", Api.move Backward
      ; "move_left", Api.move Left
      ; "move_right", Api.move Right
      ; "attack", Api.attack
      ; "turn", Api.turn_body_part false "turn_right"
      ; "turn_right", Api.turn_body_part false "turn_right"
      ; "turn_left", Api.turn_body_part true "turn_right"
      ; "turn_head", Api.turn_body_part false "turn_head_right"
      ; "turn_head_right", Api.turn_body_part false "turn_head_right"
      ; "turn_head_left", Api.turn_body_part true "turn_head_right"
      ; "turn_arms", Api.turn_body_part false "turn_arms_right"
      ; "turn_arms_right", Api.turn_body_part false "turn_arms_right"
      ; "turn_arms_left", Api.turn_body_part true "turn_arms_right"
      ; "log", Api.log name
      ]
  ;;

  let load_implementation path name get_player_info =
    let lua_state = lua_load_player_from_file path in
    create_lua_api lua_state name get_player_info;
    make_lua_player lua_state
  ;;

  let read_meta dir =
    let ls = Lua.newstate () in
    if Lua.dofile ls (Filename.concat dir "meta.lua")
    then (
      Lua.getglobal ls "name";
      match Lua.tostring ls (-1) with
      | None ->
        Printf.printf "WARNING - Missing player name in %s\n%!" dir;
        None
      | Some name ->
        Lua.getglobal ls "version";
        let version = Option.value (Lua.tostring ls (-1)) ~default:"1.0" in
        Lua.getglobal ls "entrypoint";
        let entrypoint = Option.value (Lua.tostring ls (-1)) ~default:"main.lua" in
        let color = Option.value (lua_get_color ls) ~default:(Color.random ()) in
        Some { name; color; version; entrypoint })
    else (
      Printf.printf "WARNING - Missing meta.lua in %s/\n%!" dir;
      None)
  ;;
end
