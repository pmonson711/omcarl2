(* tokens only *)
%token BOOL
%token INT
%token NAT
%token POS
%token REAL
%token LIST
%token BAG
%token FBAG
%token SET
%token FSET
%token STRUCT
%token <string> ID
%token <int> NUMBER

(* %token SORT_SEC *)
(* %token CONS_SEC *)
(* %token MAP_SEC *)
(* %token VAR_SEC *)
(* %token EQN_SEC *)

(* DATA SECTION *)
%token TRUE "true"
%token FALSE "false"
%token L_BRACE "[" R_BRACE "]"
%token L_BRACK "{" R_BRACK "}"

%token QUESTION "?"
%token EQUAL "="
%token BAR "|"
%token LPARAN "(" RPARAN ")"
%token RARROW "->"
%token DRARROW "=>"
%token HASH "#"
%token COMMA ","
%token EXCLAIM "!"
%token COLON ":"
%token SEMI_COlON ";"
%token NEGATION "-"
%token FORALL "forall"
%token EXISTS "exists"
%token LAMBDA "lambda"
%token DOT "."
%token DBAR "||"
%token EOF

%%
