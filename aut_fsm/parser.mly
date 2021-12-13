%token <string> STR
%token <string> Q_STR
%token <int> POS
%token L_PARAM "("
%token R_PARAM ")"
%token DES
%token EOL
%token EOF

%{ open Grammar %}
%start <Grammar.Fsm.t> fsm
%%

header:
  | DES
  ; L_PARAM
  ; first_state= POS
  ; nr_of_transitions= POS
  ; nr_of_states= POS
  ; R_PARAM
    { Header.make
        ~kind: Des
        ~first_state: (State.make first_state ())
        ~nr_of_transitions: nr_of_transitions
        ~nr_of_states: nr_of_states
    }

fsm:
  | EOL?
  ; header= header
  ; EOF
      { Header.t { header
        ; states= []
        ; transistions= []
        }
      }
