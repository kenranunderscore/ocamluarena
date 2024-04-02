let () =
  let open Alcotest in
  run
    "unit tests"
    [ "math utilities", Test_math.tests; "game engine stuff", Test_game.tests ]
;;
