include Lua_api.LuaL
include Lua_api.Lua

(* For some reason LuaL.register segfaults and/or runs into an allocation/GC
   error?? This one works instead *)
let pushmodule ls library_name functions =
  Lua_api.Lua.newtable ls;
  List.iter
    (fun (name, f) ->
      Lua_api.Lua.pushcfunction ls f;
      Lua_api.Lua.setfield ls (-2) name)
    functions;
  Lua_api.Lua.setglobal ls library_name
;;

(** Like Lua_api.Lua.call but with assertions for the non-negativity of its
    integer arguments *)
let call ls nargs nreturns =
  assert (nargs >= 0);
  assert (nreturns >= 0);
  Lua_api.Lua.call ls nargs nreturns
;;
