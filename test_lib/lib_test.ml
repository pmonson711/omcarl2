let () =
  let open Alcotest in
  run "Mcrl2 Parser" [ Sort_test.case; Cons_test.case; Data_test.case ]
