open Alcotest
module Point = Arena.Point

let test_can_spot_1st_quadrant_too_far_left () =
  let open Arena.Game in
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
  let open Arena.Game in
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
  let open Arena.Game in
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
  let open Arena.Game in
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

let tests =
  [ test_case "can_spot" `Quick test_can_spot_1st_quadrant_too_far_left
  ; test_case "can spot" `Quick test_can_spot_1st_quadrant_too_far_right
  ; test_case "can spot" `Quick test_can_spot_1st_quadrant_head_on
  ; test_case "can spot" `Quick test_can_spot_1st_quadrant_behind
  ]
;;
