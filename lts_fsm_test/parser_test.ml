let test_name = "Parser"

let fsm =
  let open Lts_fsm in
  let open Alcotest in
  Grammar.(testable pp equal)

let parse str = str |> Lts_fsm.parse_from_string

let empty () =
  let open Alcotest in
  check (result fsm string) "empty is none"
    (Error "Syntax error -1 on line 1, character 1: Invalid syntax") (parse "")

let simple_switch () =
  let text =
    {|
    state(2) State "Off" "On"
    ---
    0
    1
    ---
    0 1 "flick"
    1 0 "flick"
  |}
  in
  let open Alcotest in
  let open Lts_fsm.Grammar in
  check (result fsm string) "expected sitch"
    (Ok
       { parameters=
           [ { parameter_name= "state"
             ; domain_cardinality= 2
             ; domain_name= "State"
             ; values= [ "Off"; "On" ]
             }
           ]
       ; states= [ [ 0 ]; [ 1 ] ]
       ; transistions=
           [ { source_state= 0; target_state= 1; label= "flick" }
           ; { source_state= 1; target_state= 0; label= "flick" }
           ]
       })
    (parse text)

let suite =
  let open Alcotest in
  ( test_name
  , [ test_case "Empty" `Quick empty
    ; test_case "Simple switch" `Quick simple_switch
    ] )
