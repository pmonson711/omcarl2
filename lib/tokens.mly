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

%token BAR "|"
%token COLON ":"
%token COMMA ","
%token DAMP "&&"
%token DBAR "||"
%token DOT "."
%token DRARROW "=>"
%token EQUAL "="
%token EQUALITY "=="
%token EXCLAIM "!"
%token EXISTS "exists"
%token FORALL "forall"
%token GREATERTHAN ">"
%token GREATERTHANEQUAL ">="
%token HASH "#"
%token INEQUALITY "!="
%token LAMBDA "lambda"
%token LESSTHAN "<"
%token LESSTHANEQUAL "<="
%token LPARAN "(" RPARAN ")"
%token NEGATION "-"
%token QUESTION "?"
%token RARROW "->"
%token SEMI_COlON ";"
%token EOF

%%
