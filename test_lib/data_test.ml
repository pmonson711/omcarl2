let test_name = "Data parser"

let data_expr =
  let open Omcrl2 in
  let open Alcotest in
  testable Mcrl2.pp_data_expr Mcrl2.equal_data_expr

let parse str =
  let open Omcrl2.Data_parser in
  let open Omcrl2.Lexer in
  str |> Lexing.from_string |> prog @@ read_tokens

let empty () =
  Alcotest.(check (option data_expr)) "empty is none" None (parse "")

let basics () =
  let data_check msg to_parse t =
    Alcotest.(check (option data_expr)) msg t (parse to_parse)
  in
  data_check "Empty set" "{}" (Some (`Set [])) ;
  data_check "Empty set" "[]" (Some (`List [])) ;
  data_check "Empty bag" "{:}" (Some (`Bag [])) ;
  data_check "Some set" "{1}" (Some (`Set [ `Number 1 ])) ;
  data_check "Some set" "{1,2}" (Some (`Set [ `Number 1; `Number 2 ])) ;
  data_check "Some list" "[1]" (Some (`List [ `Number 1 ])) ;
  data_check "Some list" "[1,2]" (Some (`List [ `Number 1; `Number 2 ])) ;
  data_check "Some list" "{a: Int | true}"
    (Some (`SetComprehension (`VarDecl ("a", `Integer), `True))) ;
  data_check "Some bag" "{true: 1}"
    (Some (`Bag [ `BagEnumElt (`True, `Number 1) ])) ;
  data_check "Some bag" "{1: 1}"
    (Some (`Bag [ `BagEnumElt (`Number 1, `Number 1) ])) ;
  List.iter
    (fun n ->
      data_check
        ("number " ^ string_of_int n)
        (string_of_int n)
        (Some (`Number n)))
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 0 ] ;
  List.iter
    (fun id -> data_check ("id " ^ id) id (Some (`Id id)))
    [ "a"; "b"; "abc" ]

let case =
  let open Alcotest in
  ( test_name
  , [ test_case "empty" `Quick empty; test_case "basics" `Quick basics ] )
