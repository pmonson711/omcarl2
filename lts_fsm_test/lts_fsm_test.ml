let () =
  let open Alcotest in
  run "FSM Parser" [ Parser_test.suite ]
