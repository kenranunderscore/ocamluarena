let lua_normalize_relative_angle ls =
  let angle = Lua.tonumber ls (-1) in
  Lua.pop ls 1;
  Lua.pushnumber ls (Math.normalize_relative_angle angle);
  1
;;

let lua_normalize_absolute_angle ls =
  let angle = Lua.tonumber ls (-1) in
  Lua.pop ls 1;
  Lua.pushnumber ls (Math.normalize_absolute_angle angle);
  1
;;

let lua_to_radians ls =
  let deg = Lua.tonumber ls (-1) in
  Lua.pop ls 1;
  Lua.pushnumber ls (Math.to_radians deg);
  1
;;

let lua_from_radians ls =
  let rad = Lua.tonumber ls (-1) in
  Lua.pop ls 1;
  Lua.pushnumber ls (Math.from_radians rad);
  1
;;

let push ls =
  Lua.pushmodule
    ls
    "utils"
    [ "normalize_relative_angle", lua_normalize_relative_angle
    ; "normalize_absolute_angle", lua_normalize_absolute_angle
    ; "to_radians", lua_to_radians
    ; "from_radians", lua_from_radians
    ]
;;
