let () =
  let open Alcotest in
  run "Aut Parser" [ Parser_test.suite ]
