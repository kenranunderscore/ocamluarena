module Rect = Tsdl.Sdl.Rect
module Event = Tsdl.Sdl.Event
module Scancode = Tsdl.Sdl.Scancode

exception Sdl_error of string

let[@inline] log msg = Tsdl.Sdl.log msg

let[@inline] unwrap_sdl = function
  | Error (`Msg e) -> raise (Sdl_error e)
  | Ok result -> result
;;

let with_sdl action =
  match Tsdl.Sdl.init Tsdl.Sdl.Init.(video + events) with
  | Error (`Msg e) ->
    log "initialization failed: %s" e;
    exit 1
  | Ok () ->
    let quit () =
      print_endline "quitting SDL...";
      Tsdl.Sdl.quit ()
    in
    Fun.protect ~finally:quit (fun () ->
      print_endline "initialized";
      action ())
;;

let with_window_and_renderer ~w ~h title action =
  let flags = Tsdl.Sdl.Window.(shown + mouse_focus) in
  match Tsdl.Sdl.create_window_and_renderer ~w ~h flags with
  | Error (`Msg e) ->
    log "window+renderer creation failed: %s" e;
    exit 1
  | Ok (window, renderer) ->
    Tsdl.Sdl.set_window_title window title;
    let free () =
      print_endline "freeing window+renderer...";
      Tsdl.Sdl.destroy_renderer renderer;
      Tsdl.Sdl.destroy_window window
    in
    Fun.protect ~finally:free (fun () ->
      print_endline "window+renderer created";
      action window renderer)
;;

let[@inline] poll_event evt = Tsdl.Sdl.poll_event (Some evt)

let[@inline] set_render_draw_color renderer ~r ~g ~b =
  Tsdl.Sdl.set_render_draw_color renderer r g b 255 |> unwrap_sdl
;;

let[@inline] render_clear renderer = Tsdl.Sdl.render_clear renderer |> unwrap_sdl

let[@inline] render_fill_rect renderer rect =
  Tsdl.Sdl.render_fill_rect renderer (Some rect) |> unwrap_sdl
;;

let[@inline] render_present renderer = Tsdl.Sdl.render_present renderer
