open Alcotest
open Arena

let dummy : Player.t =
  { name = "dummy"; color = Color.make ~red:0 ~green:0 ~blue:0; version = "1" }
;;

let test_can_spot_1st_quadrant_too_far_left () =
  let open Game in
  let res =
    can_spot
      ~player_angle_of_vision:1.4
      ~player_radius:25.
      (Point.make ~x:400. ~y:400.)
      (-.Float.pi /. 4.)
      (Point.make ~x:500. ~y:300.)
  in
  check bool "cannot spot" false res
;;

let test_can_spot_1st_quadrant_too_far_right () =
  let open Game in
  let res =
    can_spot
      ~player_angle_of_vision:1.4
      ~player_radius:25.
      (Point.make ~x:400. ~y:400.)
      (3. *. Float.pi /. 4.)
      (Point.make ~x:500. ~y:300.)
  in
  check bool "cannot spot" false res
;;

let test_can_spot_1st_quadrant_head_on () =
  let open Game in
  let res =
    can_spot
      ~player_angle_of_vision:1.4
      ~player_radius:25.
      (Point.make ~x:400. ~y:400.)
      (Float.pi /. 4.)
      (Point.make ~x:500. ~y:300.)
  in
  check bool "can spot" true res
;;

let test_can_spot_1st_quadrant_behind () =
  let open Game in
  let res =
    can_spot
      ~player_angle_of_vision:1.4
      ~player_radius:25.
      (Point.make ~x:400. ~y:400.)
      (5. *. Float.pi /. 4.)
      (Point.make ~x:500. ~y:300.)
  in
  check bool "cannot spot" false res
;;

let check_head_turned_result expected_view_direction expected_remaining = function
  | Some (Game.Event.Head_turned (_, view_direction, remaining)) ->
    check
      (pair (float 0.0001) (float 0.0001))
      "head didn't turn quite right"
      (expected_view_direction, expected_remaining)
      (view_direction, remaining)
  | Some _ -> Alcotest.fail "wrong event"
  | _ -> Alcotest.fail "no event"
;;

let test_turn_head_right_no_cap () =
  let result =
    Game.turn_head dummy ~max_view_turn_rate:0.5 ~view_direction:1. ~angle:0.2
  in
  check_head_turned_result 1.2 0. result
;;

let test_turn_head_right_with_cap () =
  let result =
    Game.turn_head dummy ~max_view_turn_rate:0.4 ~view_direction:0.3 ~angle:0.9
  in
  check_head_turned_result 0.7 0.5 result
;;

let test_turn_head_right_with_limit () =
  let result =
    Game.turn_head dummy ~max_view_turn_rate:0.5 ~view_direction:1.5 ~angle:0.4
  in
  check_head_turned_result Math.half_pi 0. result
;;

let test_turn_head_right_remaining_angle_is_shortened () =
  let result =
    Game.turn_head dummy ~max_view_turn_rate:0.1 ~view_direction:1.3 ~angle:0.9
  in
  check_head_turned_result 1.4 (Math.half_pi -. 1.4) result
;;

let test_turn_head_left_no_cap () =
  let result =
    Game.turn_head dummy ~max_view_turn_rate:0.5 ~view_direction:1. ~angle:(-0.2)
  in
  check_head_turned_result 0.8 0. result
;;

let test_turn_head_left_with_cap () =
  let result =
    Game.turn_head dummy ~max_view_turn_rate:0.1 ~view_direction:1.3 ~angle:(-0.3)
  in
  check_head_turned_result 1.2 (-0.2) result
;;

let test_turn_head_left_with_limit () =
  let result =
    Game.turn_head dummy ~max_view_turn_rate:0.5 ~view_direction:(-1.5) ~angle:(-0.7)
  in
  check_head_turned_result (-.Math.half_pi) 0. result
;;

let test_turn_head_left_remaining_angle_is_shortened () =
  let result =
    Game.turn_head dummy ~max_view_turn_rate:0.2 ~view_direction:(-0.9) ~angle:(-0.8)
  in
  check_head_turned_result (-1.1) (1.1 -. Math.half_pi) result
;;

let test_turn_head_noop () =
  let result =
    Game.turn_head dummy ~max_view_turn_rate:1. ~view_direction:(-1.1) ~angle:0.
  in
  check bool "head turned unexpectedly" true (Option.is_none result)
;;

let test_turn_head_noop_if_already_at_right_limit () =
  let result =
    Game.turn_head dummy ~max_view_turn_rate:1. ~view_direction:Math.half_pi ~angle:0.1
  in
  check bool "head turned unexpectedly" true (Option.is_none result)
;;

let tests =
  [ test_case "can_spot" `Quick test_can_spot_1st_quadrant_too_far_left
  ; test_case "can spot" `Quick test_can_spot_1st_quadrant_too_far_right
  ; test_case "can spot" `Quick test_can_spot_1st_quadrant_head_on
  ; test_case "can spot" `Quick test_can_spot_1st_quadrant_behind
  ; test_case "not turning the head" `Quick test_turn_head_noop
  ; test_case
      "trying to turn at limit"
      `Quick
      test_turn_head_noop_if_already_at_right_limit
  ; test_case "turning head to the right, cap not hit" `Quick test_turn_head_right_no_cap
  ; test_case "turning head to the right, cap hit" `Quick test_turn_head_right_with_cap
  ; test_case
      "turning head to the right, limit hit"
      `Quick
      test_turn_head_right_with_limit
  ; test_case
      "turning head to the right, remaining angle shortened if target is higher than \
       right limit"
      `Quick
      test_turn_head_right_remaining_angle_is_shortened
  ; test_case "turning head to the left, cap not hit" `Quick test_turn_head_left_no_cap
  ; test_case "turning head to the left, cap hit" `Quick test_turn_head_left_with_cap
  ; test_case "turning head to the left, limit hit" `Quick test_turn_head_left_with_limit
  ; test_case
      "turning head to the left, remaining angle shortened if target is lower than left \
       limit"
      `Quick
      test_turn_head_left_remaining_angle_is_shortened
  ]
;;
