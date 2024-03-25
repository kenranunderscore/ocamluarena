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

let with_window ~w ~h title action =
  let flags = Tsdl.Sdl.Window.(shown + mouse_focus) in
  match Tsdl.Sdl.create_window title ~w ~h flags with
  | Error (`Msg e) ->
    log "window creation failed: %s" e;
    exit 1
  | Ok window ->
    let free () =
      print_endline "freeing window...";
      Tsdl.Sdl.destroy_window window
    in
    Fun.protect ~finally:free (fun () ->
      print_endline "window created";
      action window)
;;

let with_renderer window action =
  let flags = Tsdl.Sdl.Renderer.accelerated in
  match Tsdl.Sdl.create_renderer ~flags window with
  | Error (`Msg e) ->
    log "renderer creation failed: %s" e;
    exit 1
  | Ok renderer ->
    let free () =
      print_endline "freeing renderer...";
      Tsdl.Sdl.destroy_renderer renderer
    in
    Fun.protect ~finally:free (fun () ->
      print_endline "renderer created";
      action renderer)
;;

let with_window_and_renderer ~w ~h title action =
  with_window ~w ~h title (fun w -> with_renderer w (fun r -> action w r))
;;

let[@inline] poll_event evt = Tsdl.Sdl.poll_event (Some evt)

let[@inline] set_render_draw_color renderer ~red ~green ~blue =
  Tsdl.Sdl.set_render_draw_color renderer red green blue 255 |> unwrap_sdl
;;

let[@inline] render_clear renderer = Tsdl.Sdl.render_clear renderer |> unwrap_sdl

let[@inline] render_fill_rect renderer rect =
  Tsdl.Sdl.render_fill_rect renderer (Some rect) |> unwrap_sdl
;;

let[@inline] render_draw_point renderer ~x ~y =
  Tsdl.Sdl.render_draw_point_f renderer x y |> unwrap_sdl
;;

let[@inline] render_present renderer = Tsdl.Sdl.render_present renderer

let draw_circle renderer (p : Point.t) radius =
  let px, py = p.x, p.y in
  let diameter = radius *. 2. in
  let x = ref (radius -. 1.) in
  let y = ref 0. in
  let tx = ref 1. in
  let ty = ref 1. in
  let terror = ref (!tx -. diameter) in
  while x >= y do
    render_draw_point renderer ~x:(px +. !x) ~y:(py -. !y);
    render_draw_point renderer ~x:(px +. !x) ~y:(py +. !y);
    render_draw_point renderer ~x:(px -. !x) ~y:(py -. !y);
    render_draw_point renderer ~x:(px -. !x) ~y:(py +. !y);
    render_draw_point renderer ~x:(px +. !y) ~y:(py -. !x);
    render_draw_point renderer ~x:(px +. !y) ~y:(py +. !x);
    render_draw_point renderer ~x:(px -. !y) ~y:(py -. !x);
    render_draw_point renderer ~x:(px -. !y) ~y:(py +. !x);
    if !terror <= 0.
    then (
      y := !y +. 1.;
      terror := !terror +. !ty;
      ty := !ty +. 2.)
    else (
      x := !x -. 1.;
      tx := !tx +. 2.;
      terror := !terror +. !tx -. diameter)
  done
;;

let[@inline] draw_line renderer (p : Point.t) (q : Point.t) =
  Tsdl.Sdl.render_draw_line_f renderer p.x p.y q.x q.y |> unwrap_sdl
;;

let[@inline] scale renderer factor =
  Tsdl.Sdl.render_set_scale renderer factor factor |> unwrap_sdl
;;
