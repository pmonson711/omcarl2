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
%token QUESTION
%token EQUAL
%token BAR
%token LPARAN
%token RPARAN
%token RARROW
%token HASH
%token COMMA
%token COLON
%token SEMI_COlON
%token EOF

%left RARROW
%left HASH

%start <Mcrl2.t option> prog
%%

prog:
  | EOF       { None }
  | v = value { Some v }

value:
  | exp= sort_decl; SEMI_COlON                     { `SortDecl exp }

sort_exp:
  | BOOL                                           { `Boolean }
  | INT                                            { `Integer }
  | NAT                                            { `Natural }
  | POS                                            { `Positive }
  | REAL                                           { `Real } 
  | STRUCT; def= constr_decl_list;                 { `Struct def } 
  | LIST; LPARAN; exp = sort_exp; RPARAN           { `List exp }
  | BAG;  LPARAN; exp = sort_exp; RPARAN           { `Bag exp }
  | SET;  LPARAN; exp = sort_exp; RPARAN           { `Set exp }
  | FBAG; LPARAN; exp = sort_exp; RPARAN           { `FiniteBag exp }
  | FSET; LPARAN; exp = sort_exp; RPARAN           { `FiniteSet exp }
  | LPARAN; exp = sort_exp; RPARAN                 { exp }
  | id = ID                                        { `Ref id }
  | e1= sort_exp; op= sort_bin_op; e2= sort_exp    { `TypeOp (op, e1, e2) }

constr_decl:
  (* | id= ID; proj= proj_decl                        { `TConstr (id, [proj], None) } *)
  | id= ID; LPARAN; proj= proj_decl_list; RPARAN; 
            QUESTION; q= ID                        { `TConstr (id, proj, Some q) }
  | id= ID; LPARAN; proj= proj_decl_list; RPARAN;  { `TConstr (id, proj, None) }
  | id= ID; QUESTION; q= ID                        { `Constr (id, Some q) }
  | id= ID;                                        { `Constr (id, None) }

constr_decl_list:
  | lst= separated_nonempty_list(BAR, constr_decl) { lst }

sort_decl:
  | lst= id_list                                   { `IdList lst }
  | id= ID; EQUAL; def= sort_exp                   { `Id (id, def) }

proj_decl:
  | id= ID; COLON; exp= sort_exp                   { `TDecl (id, exp) }
  | id= ID;                                        { `Decl id }

proj_decl_list:
  | lst= separated_nonempty_list(COMMA, proj_decl) { lst }

id_list:
  | lst= separated_nonempty_list(COMMA, ID)        { lst }

%inline sort_bin_op:
  | RARROW                                         { Mcrl2.Lambda }
  | HASH                                           { Mcrl2.Tuple }
