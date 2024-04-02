open Alcotest
module Math = Arena.Math
module Point = Arena.Point

let test_sign_negative_input () =
  check (float 0.00001) "negative input" (-1.) (Math.sign (-5.))
;;

let test_sign_positive_input () = check (float 0.00001) "positive input" 1. (Math.sign 5.)

let test_sign_negative_zero () =
  check (float 0.00001) "negative zero" (-1.) (Math.sign (-0.))
;;

let test_sign_positive_zero () = check (float 0.00001) "negative zero" 1. (Math.sign 0.)

let test_circles_intersect_too_far_apart () =
  let p = Point.make ~x:0. ~y:0. in
  let q = Point.make ~x:100. ~y:0. in
  let res = Math.circles_intersect p 49. q 50. in
  check bool "no intersection" false res
;;

let test_circles_intersect_touch () =
  let p = Point.make ~x:0. ~y:0. in
  let q = Point.make ~x:100. ~y:0. in
  let res = Math.circles_intersect p 50. q 50. in
  check bool "no intersection" true res
;;

let test_circles_intersect_overlap () =
  let p = Point.make ~x:0. ~y:0. in
  let q = Point.make ~x:100. ~y:0. in
  let res = Math.circles_intersect p 100. q 20. in
  check bool "no intersection" true res
;;

let test_normalize_absolute_angle_greater_than_2pi () =
  let res = Math.normalize_absolute_angle 7. in
  check (float 0.0001) "between 0 and 2pi" 0.71682 res
;;

let test_normalize_absolute_angle_greater_than_4pi () =
  let res = Math.normalize_absolute_angle (5. *. Float.pi) in
  check (float 0.0001) "between 0 and 2pi" Float.pi res
;;

let test_normalize_absolute_angle_less_than_0 () =
  let res = Math.normalize_absolute_angle (-.Float.pi) in
  check (float 0.0001) "between 0 and 2pi" Float.pi res
;;

let test_normalize_absolute_angle_less_than_minus_2pi () =
  let res = Math.normalize_absolute_angle (-5. *. Float.pi) in
  check (float 0.0001) "between 0 and 2pi" Float.pi res
;;

let test_normalize_absolute_angle_between_0_and_2pi () =
  let res = Math.normalize_absolute_angle 4.123 in
  check (float 0.0001) "doesn't change the angle" 4.123 res
;;

let test_normalize_relative_angle_greater_than_pi () =
  let res = Math.normalize_relative_angle 7. in
  check (float 0.0001) "between -pi and pi" 0.71682 res
;;

let test_normalize_relative_angle_less_than_minus_pi () =
  let res = Math.normalize_relative_angle (-2. *. Float.pi) in
  check (float 0.0001) "between -pi and pi" 0. res
;;

let test_normalize_relative_angle_between_minus_pi_and_pi () =
  let res = Math.normalize_relative_angle 1.123 in
  check (float 0.0001) "doesn't change the angle" 1.123 res
;;

let test_normalize_relative_angle_between_pi_and_2pi () =
  let res = Math.normalize_relative_angle (3. /. 2. *. Float.pi) in
  check (float 0.0001) "maps to negative side" (-.Float.pi /. 2.) res
;;

let tests =
  [ test_case "sign" `Quick test_sign_negative_input
  ; test_case "sign" `Quick test_sign_positive_input
  ; test_case "sign" `Quick test_sign_negative_zero
  ; test_case "sign" `Quick test_sign_positive_zero
  ; test_case "circles_intersect" `Quick test_circles_intersect_too_far_apart
  ; test_case "circles_intersect" `Quick test_circles_intersect_touch
  ; test_case "circles_intersect" `Quick test_circles_intersect_overlap
  ; test_case
      "normalizing absolute angle >= 2 pi"
      `Quick
      test_normalize_absolute_angle_greater_than_2pi
  ; test_case
      "normalizing absolute angle > 4 pi"
      `Quick
      test_normalize_absolute_angle_greater_than_4pi
  ; test_case
      "normalizing absolute angle < 0"
      `Quick
      test_normalize_absolute_angle_less_than_0
  ; test_case
      "normalizing absolute angle < -2pi"
      `Quick
      test_normalize_absolute_angle_less_than_minus_2pi
  ; test_case
      "normalizing absolute angle in [0, 2pi["
      `Quick
      test_normalize_absolute_angle_between_0_and_2pi
  ; test_case
      "normalizing relative angle >= pi"
      `Quick
      test_normalize_relative_angle_greater_than_pi
  ; test_case
      "normalizing relative angle < -pi"
      `Quick
      test_normalize_relative_angle_less_than_minus_pi
  ; test_case
      "normalizing relative angle in [-pi, pi["
      `Quick
      test_normalize_relative_angle_between_minus_pi_and_pi
  ; test_case
      "normalizing relative angle in [pi, 2pi["
      `Quick
      test_normalize_relative_angle_between_pi_and_2pi
  ]
;;
