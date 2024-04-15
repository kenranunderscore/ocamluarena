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

let test_turn_head_right_no_overflow_no_cap () =
  let result =
    Game.turn_head dummy ~max_view_turn_rate:0.5 ~view_direction:1. ~angle:0.2
  in
  check_head_turned_result 1.2 0. result
;;

let test_turn_head_right_with_overflow_no_cap () =
  let result =
    Game.turn_head dummy ~max_view_turn_rate:1.5 ~view_direction:5.9 ~angle:0.8
  in
  check_head_turned_result (6.7 -. Math.two_pi) 0. result
;;

let test_turn_head_right_no_overflow_with_cap () =
  let result =
    Game.turn_head dummy ~max_view_turn_rate:0.9 ~view_direction:0.3 ~angle:2.0
  in
  check_head_turned_result 1.2 1.1 result
;;

let test_turn_head_right_with_overflow_with_cap () =
  let result =
    Game.turn_head dummy ~max_view_turn_rate:0.5 ~view_direction:6.0 ~angle:0.9
  in
  check_head_turned_result (6.5 -. Math.two_pi) 0.4 result
;;

let test_turn_head_left_no_underflow_no_cap () =
  let result =
    Game.turn_head dummy ~max_view_turn_rate:0.5 ~view_direction:1. ~angle:(-0.2)
  in
  check_head_turned_result 0.8 0. result
;;

let test_turn_head_left_with_underflow_no_cap () =
  let result =
    Game.turn_head dummy ~max_view_turn_rate:1.5 ~view_direction:0.5 ~angle:(-1.)
  in
  check_head_turned_result (-0.5 +. Math.two_pi) 0. result
;;

let test_turn_head_left_no_underflow_with_cap () =
  let result =
    Game.turn_head dummy ~max_view_turn_rate:0.9 ~view_direction:3.3 ~angle:(-1.3)
  in
  check_head_turned_result 2.4 (-0.4) result
;;

let test_turn_head_left_with_underflow_with_cap () =
  let result =
    Game.turn_head dummy ~max_view_turn_rate:0.3 ~view_direction:0. ~angle:(-1.0)
  in
  check_head_turned_result (-0.3 +. Math.two_pi) (-0.7) result
;;

let test_turn_head_noop () =
  let result = Game.turn_head dummy ~max_view_turn_rate:1. ~view_direction:2. ~angle:0. in
  check bool "head turned unexpectedly" true (Option.is_none result)
;;

let tests =
  [ test_case "can_spot" `Quick test_can_spot_1st_quadrant_too_far_left
  ; test_case "can spot" `Quick test_can_spot_1st_quadrant_too_far_right
  ; test_case "can spot" `Quick test_can_spot_1st_quadrant_head_on
  ; test_case "can spot" `Quick test_can_spot_1st_quadrant_behind
  ; test_case "not turning the head" `Quick test_turn_head_noop
  ; test_case
      "turning head to the right, no overflow, cap not hit"
      `Quick
      test_turn_head_right_no_overflow_no_cap
  ; test_case
      "turning head to the right, with overflow, cap not hit"
      `Quick
      test_turn_head_right_with_overflow_no_cap
  ; test_case
      "turning head to the right, no overflow, cap hit"
      `Quick
      test_turn_head_right_no_overflow_with_cap
  ; test_case
      "turning head to the right, with overflow, cap hit"
      `Quick
      test_turn_head_right_with_overflow_with_cap
  ; test_case
      "turning head to the left, no underflow, cap not hit"
      `Quick
      test_turn_head_left_no_underflow_no_cap
  ; test_case
      "turning head to the left, with underflow, cap not hit"
      `Quick
      test_turn_head_left_with_underflow_no_cap
  ; test_case
      "turning head to the left, no underflow, cap hit"
      `Quick
      test_turn_head_left_no_underflow_with_cap
  ; test_case
      "turning head to the left, with underflow, cap hit"
      `Quick
      test_turn_head_left_with_underflow_with_cap
  ]
;;
