%token DIVIDER "---"
%token <string> STR
%token <int> POS
%token L_PARAM "("
%token R_PARAM ")"
%token D_QUOTE
%token EOL
%token EOF

%{ open Grammer %}
%start <Grammer.t option> prog
%start <Grammer.parameter> parameter
%start <Grammer.state> state
%start <Grammer.transistion> transistion
%%


prog:
  | EOF { None }
  | f = fsm { Some f }

parameter:
  | parameter_name= STR
  ; domain_cardinality= delimited("(", POS, ")")
  ; domain_name= STR
  ; values= delimited(D_QUOTE, STR, D_QUOTE)+
  ; EOL
      { { parameter_name
        ; domain_cardinality
        ; domain_name
        ; values
        } }

state:
  | states= POS*; EOL { states }

transistion:
  | source_state= POS
  ; target_state= POS
  ; label= delimited(D_QUOTE, STR, D_QUOTE)
  ; EOL
      { { source_state
        ; target_state
        ; label
        } }

fsm:
  | EOL?
  ; parameters= parameter*
  ; "---"; EOL
  ; states = state*
  ; "---"; EOL
  ; transistions= transistion+
  ; EOF
      { { parameters
        ; states
        ; transistions
        } }
