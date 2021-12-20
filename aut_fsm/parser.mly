%token <string> Q_STR
%token <int> POS
%token L_PARAM "("
%token R_PARAM ")"
%token COMMA ","
%token DES
%token EOF

%{ open Grammar %}
%start <Grammar.Fsm.t> fsm
%%

header:
  | DES
  ; L_PARAM
  ; first_state= POS
  ; COMMA
  ; nr_of_transistions= POS
  ; COMMA
  ; nr_of_states= POS
  ; R_PARAM
    { Header.make
        ~kind: Des
        ~first_state: (State.make first_state)
        ~nr_of_transistions
        ~nr_of_states
    }

edge:
  | L_PARAM
  ; start_state= POS
  ; COMMA
  ; label= Q_STR
  ; COMMA
  ; end_state= POS
  ; R_PARAM
    { Edge.make
        ~start_state: (State.make start_state)
        ~label
        ~end_state: (State.make end_state)
    }

fsm:
  | header= header
  ; edges= edge*
  ; EOF
    { Fsm.make
        ~header
        ~edges
        ()
    }
