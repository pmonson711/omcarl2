%token <string> STR
%token <string> ID
%token <int> NUMBER
(** Sections *)
%token ACT
%token CONS
%token EQN
%token GLOB
%token INIT
%token PROC
%token SORT
(** Char *)
%token COLON ":"
%token COMMA ","
%token EQUAL "="
%token L_BRACE "{"
%token L_BRACK "["
%token L_PARAN "("
%token PERCENT "%"
%token Q_MARK "?"
%token R_BRACE "}"
%token R_BRACK "]"
%token R_PARAN ")"
%token SEMICOLON ";"
%token V_BAR "|"
%token EXCLAIM "!"
%token MINUS "-"
%token DOT "."
(** Sort Terminals *)
%token S_BOOL
%token S_POS
%token S_NAT
%token S_INT
%token S_REAL
%token S_LIST
%token S_BAG
%token S_FSET
%token S_FBAG
%token STRUCT
%token TRUE
%token FALSE
%token FORALL
%token EXISTS
%token LAMBDA
(** Infix Terminals *)
%token R_ARROW "->"
%token R_FARROW "=>"
%token HASH "#"
%token PROCEXP
%token EOF
%token EOL

%left R_ARROW
%left HASH

%{ open Grammar %}
%start <Spec.t> spec
%start <data_expr> data_spec
%%

let comment ==
    | "%"+; text= STR;                           { make_comment ~text }
    | "%"+; text= ID;                            { make_comment ~text }

let id == i= ID; ":";                            { i }
let pw(x) := | "("; x = x; ")";                  { x }
let guard == | "?"; id= id;                      { id }
let ending_comma(x) := | x= x; ";";              { x }
let c_lst(x) ==
    lst= separated_nonempty_list(",", x);        { lst }
let b_lst(x) ==
    lst= separated_nonempty_list("|", x);        { lst }
let id_list == | lst= c_lst(ID);                 { lst }

let init :=
    | INIT; PROCEXP; ";";                        { ProcExpr }

(** Sort Specifications *)
let proj_decl :=
    | i= option(id); sort_expr= sort_expr;       { { proj_id= i ; sort_expr } }

let proj_decl_list == lst= c_lst(proj_decl);     { lst }

let constr_decl :=
    | i= id; p= pw(proj_decl_list); g= guard?;   { { const_id= i
                                                   ; proj_decls= p
                                                   ; guard= g
                                                   } }

let constr_decl_list == lst= b_lst(constr_decl); { lst }

let sort_expr :=
    | S_BOOL;                                    { Bool }
    | S_POS;                                     { Pos }
    | S_NAT;                                     { Nat }
    | S_INT;                                     { Int }
    | S_REAL;                                    { Real }
    | S_LIST; t= pw(sort_expr);                  { List t }
    | S_BAG; t= pw(sort_expr);                   { List t }
    | S_FSET; t= pw(sort_expr);                  { List t }
    | S_FBAG; t= pw(sort_expr);                  { List t }
    | id= ID;                                    { Id id }
    | sort_expr= pw(sort_expr);                  { SubExpr sort_expr }
    | STRUCT; lst= constr_decl_list;             { Struct lst }
    | rh= sort_expr; "->"; lh= sort_expr;        { Function (rh, lh) }
    | rh= sort_expr; "#"; lh= sort_expr;         { Tuple (rh, lh) }

let sort_decl :=
    | id= ID; ";";                               { IdList [id] }
    | lst= id_list; ";";                         { IdList lst }
    | id= ID; "="; s= sort_expr; ";";            { SortType ( make_sort_type ~id ~signature:s ) }

let ids_decl :=
    | lst= id_list; ":"; sort_expr= sort_expr;   { make_ids_decl ~id_list:lst ~sort_expr () }

(** Data Specifications *)
let data_spec := e= ending_comma(data_expr);     { e }
let data_expr :=
    | id= ID;                                    { Id id }
    | num= NUMBER;                               { Number num }
    | TRUE;                                      { Bool true }
    | FALSE;                                     { Bool false }
    | "["; "]";                                  { List [] }
    | "{"; "}";                                  { Set [] }
    | "{"; ":"; "}";                             { Bag [] }
    | "["; expr= c_lst(data_expr); "]";          { List expr }
    | "{"; expr= c_lst(bag_enum_elt); "}";       { Bag expr }
    | "{"; v= var_decl; "|"; e= data_expr; "}";  { SetComp (v, e) }
    | "{"; expr= c_lst(data_expr); "}";          { Set expr }
    | expr= pw(data_expr);                       { SubExpr expr }
    | e1= data_expr; "["; e2= data_expr; "->"; e3= data_expr; "]";
                                                 { SetUpdate (e1, e2, e3) }
    | e1= data_expr; "("; lst= c_lst(data_expr); ")";
                                                 { Access (e1, lst) }
    | "!"; exp= data_expr;                       { Neg exp }
    | "-"; exp= data_expr;                       { Invert exp }
    | "#"; exp= data_expr;                       { Count exp }
    | FORALL; v= c_lst(var_decl); "."; e= data_expr;
                                                 { ForAll (v, e) }
    | EXISTS; v= c_lst(var_decl); "."; e= data_expr;
                                                 { Exists (v, e) }
    | LAMBDA; v= c_lst(var_decl); "."; e= data_expr;
                                                 { Lambda (v, e) }

let bag_enum_elt :=
    | lh= data_expr; ":"; rh= data_expr;         { BagEnumElt (lh, rh) }

let var_decl :=
    | id= ID; ":"; expr= sort_expr;              { VarDecl (id, expr) }

(** Main Specification *)
let specs :=
    | c= comment;                                { Comment c }
    | SORT; sort_decls= sort_decl+;              { SortSpec sort_decls }
    | CONS; ids_decls= ending_comma(ids_decl)+;  { ConsSpec ids_decls }
    (* | EQN;                                       { EqnSpec } *)
    (* | GLOB;                                      { GlobalVarSpec } *)
    (* | ACT;                                       { ActSpec } *)
    (* | PROC;                                      { ProcSpec } *)

let spec :=
    | specs= specs+; init= init?; EOF;           { Spec.make ~specs ?init () }
