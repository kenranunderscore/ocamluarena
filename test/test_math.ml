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

let tests =
  [ test_case "sign" `Quick test_sign_negative_input
  ; test_case "sign" `Quick test_sign_positive_input
  ; test_case "sign" `Quick test_sign_negative_zero
  ; test_case "sign" `Quick test_sign_positive_zero
  ; test_case "circles_intersect" `Quick test_circles_intersect_too_far_apart
  ; test_case "circles_intersect" `Quick test_circles_intersect_touch
  ; test_case "circles_intersect" `Quick test_circles_intersect_overlap
  ]
;;
