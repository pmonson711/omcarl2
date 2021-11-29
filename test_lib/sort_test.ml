let test_name = "Sort_parser"

let mcrl2 =
  let open Omcrl2 in
  let open Alcotest in
  testable Mcrl2.pp Mcrl2.equal

let parse str =
  let open Omcrl2.Sort_parser in
  let open Omcrl2.Lexer in
  str |> Lexing.from_string |> prog @@ read_tokens

let is_value id t = Some (`SortDecl (`Id (id, t)))

let sort_sanity () =
  let open Omcrl2.Mcrl2 in
  Alcotest.(check mcrl2) "has sane equal" (`Section Sort) (`Section Sort)

let empty () = Alcotest.(check (option mcrl2)) "empty is none" None (parse "")

let under_defined () =
  Alcotest.(check mcrl2)
    "underdefined sort decl"
    (`SortDecl (`IdList [ "A" ]))
    (`SortDecl (`IdList [ "A" ]))

let under_defined_parse () =
  Alcotest.(check mcrl2)
    "underdefined sort decl"
    (`SortDecl (`IdList [ "A" ]))
    (parse "A ;" |> Option.get)

let primitive () =
  let parse_check str t =
    Alcotest.(check (option mcrl2))
      (str ^ " sort decl") (is_value "A" t)
      (parse ("A= " ^ str ^ " ;"))
  in
  parse_check "Pos" `Positive ;
  parse_check "Bool" `Boolean ;
  parse_check "Int" `Integer ;
  parse_check "Nat" `Natural ;
  parse_check "Real" `Real

let lists_bags_sets () =
  let parse_check str t =
    Alcotest.(check (option mcrl2))
      (str ^ " sort decl") (is_value "A" t)
      (parse ("A= " ^ str ^ " ;"))
  in
  let primitives =
    [ ("Pos", `Positive)
    ; ("Bool", `Boolean)
    ; ("Int", `Integer)
    ; ("Nat", `Natural)
    ; ("Real", `Real)
    ]
  in
  List.iter
    (fun (str, t) ->
      parse_check ("List(" ^ str ^ ")") (`List t) ;
      parse_check ("Bag(" ^ str ^ ")") (`Bag t) ;
      parse_check ("Set(" ^ str ^ ")") (`Set t) ;
      parse_check ("FSet(" ^ str ^ ")") (`FiniteSet t) ;
      parse_check ("FBag(" ^ str ^ ")") (`FiniteBag t))
    primitives

let references () =
  Alcotest.(check (option mcrl2))
    "simple reference"
    (is_value "A" (`Ref "B"))
    (parse "A= B ;")

let structs () =
  let struct_check assert_msg t to_parse =
    Alcotest.(check (option mcrl2))
      assert_msg
      (is_value "A" (`Struct t))
      (parse to_parse)
  in
  struct_check "simple underdefined struct"
    [ `Constr ("B", None) ]
    "A= struct B ;" ;
  struct_check "simple underdefined struct with type test"
    [ `Constr ("B", Some "is_b") ]
    "A= struct B ?is_b ;" ;
  struct_check "simple unkeyed struct"
    [ `TConstr ("B", [ `TDecl ("i", `Positive) ], None) ]
    "A= struct B(i: Pos) ;" ;
  struct_check "simple variant struct"
    [ `TConstr ("B", [ `TDecl ("i", `Natural) ], None); `Constr ("C", None) ]
    "A= struct B(i: Nat) | C ;"

let infix () =
  Alcotest.(check (option mcrl2))
    "simple infix"
    (is_value "A" (`TypeOp (Omcrl2.Mcrl2.Lambda, `Positive, `Boolean)))
    (parse "A= Pos -> Bool ;")

let case =
  let open Alcotest in
  ( test_name
  , [ test_case "Empty" `Quick empty
    ; test_case "sanity check" `Quick sort_sanity
    ; test_case "underdefined" `Quick under_defined
    ; test_case "Primitive types" `Quick primitive
    ; test_case "Lists, Bags, and Sets" `Quick lists_bags_sets
    ; test_case "References" `Quick references
    ; test_case "Structs" `Quick structs
    ; test_case "Infex" `Quick infix
    ] )
