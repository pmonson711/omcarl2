%token <string> STR
%token <string> ID
%token <int> NUMBER
(** Sections *)
%token ACT CONS EQN MAP VAR GLOB INIT PROC SORT
(** Char *)
%token COLON ":" SEMICOLON ";" COMMA "," V_BAR "|"
%token L_BRACE "{" L_BRACK "[" L_PARAN "("
%token R_BRACE "}" R_BRACK "]" R_PARAN ")"
%token PERCENT "%" Q_MARK "?" EXCLAIM "!"
%token MINUS "-" PLUS "+" DOT "." ASTERISK "*"
%token EQUAL "=" D_EQUAL "==" EXCLAIM_EQUAL "!="
%token D_BAR "||" D_AMP "&&"
%token GT ">" GTE ">=" LT "<" LTE "<="
%token IN "in" SNOC "|>" CONS2 "<|"
%token CONCAT "++" F_SLASH "/" DIV
(** Sort Terminals *)
%token S_BOOL S_POS S_NAT S_INT S_REAL S_LIST S_SET S_BAG S_FSET S_FBAG STRUCT
%token TRUE FALSE
%token FORALL EXISTS LAMBDA
%token WHERE "whr" END
(** Infix Terminals *)
%token R_ARROW "->" R_FARROW "=>" HASH "#"
%token PROCEXP
%token EOF

%left R_ARROW R_FARROW HASH CONCAT F_SLASH DIV GT GTE LT LTE
%left IN SNOC CONS2 D_BAR D_EQUAL  EXCLAIM_EQUAL
%right WHERE
%left L_BRACK L_PARAN
%left EXCLAIM
%right MINUS PLUS DOT ASTERISK

%{ open Grammar %}
%start <Spec.t> spec
%%
let comment ==
    | "%"+; text= STR;                           { make_comment ~text }
    | "%"+; text= ID;                            { make_comment ~text }

let id == i= ID; ":";                            { i }
let pw(x) := | "("; x = x; ")";                  { x }
let guard == | "?"; id= ID;                      { id }
let ending_semi(x) := | x= x; ";";               { x }
let s_lst(x) ==
    lst= separated_nonempty_list(";", x);        { lst }
let c_lst(x) ==
    lst= separated_nonempty_list(",", x);        { lst }
let b_lst(x) ==
    lst= separated_nonempty_list("|", x);        { lst }
let id_list == | lst= c_lst(ID);                 { lst }

let init :=
    | INIT; PROCEXP; ";";                        { ProcExpr }

(** Sort Specifications *)
let proj_decl :=
    | sort_expr= sort_expr;                      { { proj_id= None; sort_expr } }
    | i= id; sort_expr= sort_expr;               { { proj_id= Some i; sort_expr } }

let proj_decl_list == lst= c_lst(proj_decl);     { lst }

let constr_decl :=
    | i= ID;                                     { { const_id= i ; proj_decls= [] ; guard= None } }
    | i= ID; g= guard;                           { { const_id= i ; proj_decls= [] ; guard= Some g } }
    | i= ID; p= pw(proj_decl_list); g= guard?;   { { const_id= i ; proj_decls= p ; guard= g } }

let constr_decl_list == lst= b_lst(constr_decl); { lst }

let sort_expr :=
    | S_BOOL;                                    { Bool }
    | S_POS;                                     { Pos }
    | S_NAT;                                     { Nat }
    | S_INT;                                     { Int }
    | S_REAL;                                    { Real }
    | S_LIST; t= pw(sort_expr);                  { List t }
    | S_SET; t= pw(sort_expr);                   { Set t }
    | S_BAG; t= pw(sort_expr);                   { List t }
    | S_FSET; t= pw(sort_expr);                  { List t }
    | S_FBAG; t= pw(sort_expr);                  { List t }
    | id= ID;                                    { Id id }
    | sort_expr= pw(sort_expr);                  { SubExpr sort_expr }
    | STRUCT; lst= constr_decl_list;             { Struct lst }
    | rh= sort_expr; "->"; lh= sort_expr;        { Function (rh, lh) }
    | rh= sort_expr; "#"; lh= sort_expr;         { Tuple (rh, lh) }

let sort_decl :=
    | lst= id_list; ";";                         { IdList lst }
    | id= ID; "="; s= sort_expr; ";";            { SortType ( make_sort_type ~id ~signature:s ) }

let ids_decl :=
    | lst= id_list; ":"; sort_expr= sort_expr;   { make_ids_decl ~id_list:lst ~sort_expr () }

(** Data Specifications *)
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
    | FORALL; v= vars_decl_list; "."; e= data_expr;
                                                 { ForAll (v, e) }
    | EXISTS; v= vars_decl_list; "."; e= data_expr;
                                                 { Exists (v, e) }
    | LAMBDA; v= vars_decl_list; "."; e= data_expr;
                                                 { Lambda (v, e) }
    | lh= data_expr; "=>"; rh= data_expr;        { Implies (lh, rh) }
    | lh= data_expr; "||"; rh= data_expr;        { Or (lh, rh) }
    | lh= data_expr; "&&"; rh= data_expr;        { And (lh, rh) }
    | lh= data_expr; "=="; rh= data_expr;        { Equal (lh, rh) }
    | lh= data_expr; "!="; rh= data_expr;        { NotEqual (lh, rh) }
    | lh= data_expr; "<"; rh= data_expr;         { LessThan (lh, rh) }
    | lh= data_expr; "<="; rh= data_expr;        { LessThanEqual (lh, rh) }
    | lh= data_expr; ">="; rh= data_expr;        { GreaterThanEqual (lh, rh) }
    | lh= data_expr; ">"; rh= data_expr;         { GreaterThan (lh, rh) }
    | lh= data_expr; "in"; rh= data_expr;        { In (lh, rh) }
    | lh= data_expr; "|>"; rh= data_expr;        { Snoc (lh, rh) }
    | lh= data_expr; "<|"; rh= data_expr;        { Cons (lh, rh) }
    | lh= data_expr; "++"; rh= data_expr;        { Concat (lh, rh) }
    | lh= data_expr; "+"; rh= data_expr;         { Sum (lh, rh) }
    | lh= data_expr; "-"; rh= data_expr;         { Difference (lh, rh) }
    | lh= data_expr; "/"; rh= data_expr;         { Quotient (lh, rh) }
    | lh= data_expr; DIV; rh= data_expr;         { IntDivision (lh, rh) }
    | lh= data_expr; "*"; rh= data_expr;         { Product (lh, rh) }
    | lh= data_expr; "."; rh= data_expr;         { AtPosition (lh, rh) }
    | lh= data_expr; "whr"; rh= c_lst(assignment); END;
                                                 { Where (lh, rh) }

let bag_enum_elt :=
    | lh= data_expr; ":"; rh= data_expr;         { BagEnumElt (lh, rh) }

let var_decl :=
    | id= ID; ":"; expr= sort_expr;              { VarDecl (id, expr) }

let vars_decl :=
    | lst= id_list; ":"; expr= sort_expr;        { VarsDecl (lst, expr) }

let assignment :=
    | id= ID; "="; expr= data_expr;              { Assignment (id, expr) }

(** Eqn spec *)
let vars_decl_list :=
    | lst= c_lst(vars_decl);                     { lst }

let var_spec :=
    | VAR; lst= ending_semi(vars_decl_list)+;    { VarSpec lst }

let eqn_decl :=
    | e1= data_expr; "->"; e2= data_expr; "="; e3= data_expr; ";";
                                                 { EqnDecl (Some e1, Some e2, e3) }
    | e1= data_expr; "->";  e3= data_expr; ";";  { EqnDecl (Some e1, None, e3) } (** diverges from the BNF *)
    | e2= data_expr; "="; e3= data_expr; ";";    { EqnDecl (None, Some e2, e3) }
    (** Guessing, but not in BNF *)

(** Process Spec *)
let act_decl :=
    | lst= id_list; ";";                         { IdList lst }
    | lst= id_list; ":"; exp= sort_expr; ";";    { SortProduct (lst, exp) } (** devation from the BNF of the product *)

(** Main Specification *)
let specs :=
    | c= comment;                                { Comment c }
    | SORT; sort_decls= sort_decl+;              { SortSpec sort_decls }
    | CONS; ids_decls= ending_semi(ids_decl)+;   { ConsSpec ids_decls }
    | MAP; ids_decl= ending_semi(ids_decl)+;     { MapSpec ids_decl }
    | v= var_spec?; EQN; d= eqn_decl+;           { EqnSpec (v, d) }
    | GLOB; lst= ending_semi(vars_decl_list)+;   { GlobalVarSpec lst }
    | ACT; decl= act_decl+;                      { ActSpec decl }
    | PROC;                                      { ProcSpec }

let spec :=
    | specs= specs+; init= init?; EOF;           { Spec.make ~specs ?init () }
