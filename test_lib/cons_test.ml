let test_name = "Cons_parser"

let mcrl2 =
  let open Omcrl2 in
  let open Alcotest in
  testable Mcrl2.pp Mcrl2.equal

let parse str =
  let open Omcrl2.Cons_parser in
  let open Omcrl2.Lexer in
  str |> Lexing.from_string |> prog @@ read_tokens

let is_value id t = Some (`ConsDecl (`IdList ([ id ], Some t)))

let is_list_value ids t = Some (`ConsDecl (`IdList (ids, Some t)))

let empty () = Alcotest.(check (option mcrl2)) "empty is none" None (parse "")

let simple () =
  let open Omcrl2 in
  let cons_check msg to_parse id t =
    Alcotest.(check (option mcrl2)) msg (is_value id t) (parse to_parse)
  in
  cons_check "basic cons" "A: Pos ;" "A" `Positive ;
  cons_check "example infix" "A: Pos -> Pos ;" "A"
    (`TypeOp (Omcrl2.Mcrl2.Lambda, `Positive, `Positive)) ;
  cons_check "example infix" "one: Positive ;" "one" (`Ref "Positive") ;
  cons_check "example infix" "cdub: Bool # Positive -> Positive ;" "cdub"
    (`TypeOp
      ( Mcrl2.Lambda
      , `TypeOp (Mcrl2.Tuple, `Boolean, `Ref "Positive")
      , `Ref "Positive" ))

let simple_list () =
  let cons_check msg to_parse id t =
    Alcotest.(check (option mcrl2)) msg (is_list_value id t) (parse to_parse)
  in
  cons_check "basic cons" "A,B: Pos ;" [ "A"; "B" ] `Positive ;
  cons_check "example infix" "A,B: Pos -> Pos ;" [ "A"; "B" ]
    (`TypeOp (Omcrl2.Mcrl2.Lambda, `Positive, `Positive))

let case =
  let open Alcotest in
  ( test_name
  , [ test_case "empty" `Quick empty
    ; test_case "basic cons" `Quick simple
    ; test_case "basic lists" `Quick simple_list
    ] )
