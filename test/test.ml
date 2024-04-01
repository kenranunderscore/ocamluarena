let () =
  let open Alcotest in
  run "unit tests" [ "math utilities", Test_math.tests ]
;;
