let test_name = "Parser"

let fsm =
  let open Aut_fsm.Grammar in
  let open Alcotest in
  Fsm.(testable pp equal)

let parse str = Aut_fsm.parse_from_string str

let empty () =
  let open Alcotest in
  check (result fsm string) "empty in none"
    (Error "Syntax error -1 on line 1, character 1: Invalid syntax") (parse "")

let simple_switch () =
  let open Alcotest in
  let open Aut_fsm in
  let open Grammar in
  let text = {|
  des (0, 2, 2)
  (0, "on", 1)
  (1, "off", 2)
  |} in
  check (result fsm string) "empty in none"
    (Ok
       { Fsm.header=
           { Header.kind= Header.Des
           ; first_state= State.make 0
           ; nr_of_transistions= 2
           ; nr_of_states= 2
           }
       ; states= ListLabels.map ~f:State.make [ 2; 1; 0 ]
       ; edges=
           [ { Edge.start_state= State.make 0
             ; label= "on"
             ; end_state= State.make 1
             }
           ; { Edge.start_state= State.make 1
             ; label= "off"
             ; end_state= State.make 2
             }
           ]
       })
    (parse text)

let suite =
  let open Alcotest in
  ( test_name
  , [ test_case "Empty" `Quick empty
    ; test_case "simple switch" `Quick simple_switch
    ] )
