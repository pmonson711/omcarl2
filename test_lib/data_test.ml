let test_name = "Data_parser"

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

let data_check msg to_parse t =
  Alcotest.(check (option data_expr)) msg t (parse to_parse)

let basics () =
  data_check "Empty set" "{};" (Some (`Set [])) ;
  data_check "Empty set" "[];" (Some (`List [])) ;
  data_check "Empty bag" "{:};" (Some (`Bag [])) ;
  data_check "Some set" "{1};" (Some (`Set [ `Number 1 ])) ;
  data_check "Some set" "{1,2};" (Some (`Set [ `Number 1; `Number 2 ])) ;
  data_check "Some list" "[1];" (Some (`List [ `Number 1 ])) ;
  data_check "Some list" "[1,2];" (Some (`List [ `Number 1; `Number 2 ])) ;
  data_check "Some list" "{a: Int | true};"
    (Some (`SetComprehension (`VarDecl ("a", `Integer), `True))) ;
  data_check "Some bag" "{true: 1};"
    (Some (`Bag [ `BagEnumElt (`True, `Number 1) ])) ;
  data_check "Some bag" "{1: 1};"
    (Some (`Bag [ `BagEnumElt (`Number 1, `Number 1) ])) ;
  List.iter
    (fun n ->
      data_check
        ("number " ^ string_of_int n)
        (string_of_int n ^ ";")
        (Some (`Number n)))
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 0 ] ;
  List.iter
    (fun id -> data_check ("id " ^ id) (id ^ ";") (Some (`Id id)))
    [ "a"; "b"; "abc" ]

let set_update () =
  data_check "Some set update" "a[b -> c];"
    (Some (`SetUpdate (`Id "a", `Id "b", `Id "c")))

let function_apply () =
  data_check "Some function apply" "a(b, c);"
    (Some (`FunctionApply (`Id "a", [ `Id "b"; `Id "c" ])))

let set_complment () =
  data_check "Some set compliment" "!a;" (Some (`SetCompliment (`Id "a")))

let negation () = data_check "Some negation" "-a;" (Some (`Negation (`Id "a")))

let length () = data_check "Some length" "#a;" (Some (`Length (`Id "a")))

let forall () =
  data_check "Some forall" "forall b: Bool . (b);"
    (Some (`ForAll ([ `VarsDecl ([ "b" ], `Boolean) ], `Id "b")))

let exists () =
  data_check "Some exists" "exists b: Bool . (b);"
    (Some (`Exists ([ `VarsDecl ([ "b" ], `Boolean) ], `Id "b")))

let lambda () =
  data_check "Some lambda" "lambda b: Bool . (b);"
    (Some (`Lambda ([ `VarsDecl ([ "b" ], `Boolean) ], `Id "b")))

let case =
  let open Alcotest in
  ( test_name
  , [ test_case "empty" `Quick empty
    ; test_case "basics" `Quick basics
    ; test_case "set update" `Quick set_update
    ; test_case "function apply" `Quick function_apply
    ; test_case "set compliment" `Quick set_complment
    ; test_case "negation" `Quick negation
    ; test_case "length" `Quick length
    ; test_case "forall" `Quick forall
    ; test_case "exists" `Quick exists
    ; test_case "lambda" `Quick lambda
    ] )
