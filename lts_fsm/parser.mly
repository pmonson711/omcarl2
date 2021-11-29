%token DIVIDER "---"
%token <string> STR
%token <string> Q_STR
%token <int> POS
%token L_PARAM "("
%token R_PARAM ")"
%token EOL
%token EOF

%{ open Grammar %}
%start <Grammar.Fsm.t> fsm
%%

parameter:
  | parameter_name= STR
  ; domain_cardinality= delimited("(", POS, ")")
  ; domain_name= STR
  ; values= Q_STR+
  ; EOL
      { Parameter.make 
          ~parameter_name
          ~domain_cardinality
          ~domain_name
          ~values
          ()
      }

state:
  | states= POS*; EOL { State.make states () }

transistion:
  | source_state= POS
  ; target_state= POS
  ; label= Q_STR
  ; EOL
      { Transistion.make
          ~source_state
          ~target_state
          ~label
      }

fsm:
  | EOL?
  ; parameters= parameter*
  ; "---"; EOL
  ; states = state*
  ; "---"; EOL
  ; transistions= transistion+
  ; EOF
      { Fsm.make ~parameters
         ~states
         ~transistions
         ()
      }
