%token <string> STR
%token <string> ID
(* %token <int> NUMBER *)
%token PERCENT "%"
%token SORT
%token CONS
(* %token EQN *)
(* %token GLOB *)
(* %token ACT *)
(* %token PROC *)
%token INIT
%token SEMICOLON ";"
%token COLON ":"
%token COMMA ","
%token R_PARAN "("
%token L_PARAN ")"
%token Q_MARK "?"
%token V_BAR "|"
%token EQUAL "="
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
%token R_ARROW "->"
%token HASH "#"
(* %token DELTA *)
%token PROCEXP
%token EOF
%token EOL

%left R_ARROW
%left HASH

%{ open Grammar %}
%start <Spec.t> spec
%%

let comment == "%"+; text= STR; EOL?;            { Comment.make ~text ~loc:$loc }
let id == i= ID; ":";                            { i }
let parans(x) := | "("; x = x; ")";              { x }
let guard == | "?"; id= id;                      { id }
let ending_comma(x) := | x= x; ";";              { x }

let init :=
    | INIT; PROCEXP; ";";                        { ProcExpr }

(** Data Specifications *)
let proj_decl :=
    | i= option(id); sort_expr= sort_expr;       { { proj_id= i
                                                   ; sort_expr
                                                   } }
let proj_decl_list ==
    | lst= separated_nonempty_list(",",
                                   proj_decl);   { lst }

let constr_decl :=
    | id= id; proj_decls= parans(proj_decl_list); guard= guard?;
                                                 { { const_id= id
                                                   ; proj_decls
                                                   ; guard
                                                   } }

let constr_decl_list ==
    | lst= separated_nonempty_list("|", constr_decl);
                                                 { lst }

let sort_expr :=
    | S_BOOL;                                    { Bool }
    | S_POS;                                     { Pos }
    | S_NAT;                                     { Nat }
    | S_INT;                                     { Int }
    | S_REAL;                                    { Real }
    | S_LIST; t= parans(sort_expr);              { List t }
    | S_BAG; t= parans(sort_expr);               { List t }
    | S_FSET; t= parans(sort_expr);              { List t }
    | S_FBAG; t= parans(sort_expr);              { List t }
    | id= ID;                                    { Id id }
    | sort_expr= parans(sort_expr);              { SubExpr sort_expr }
    | STRUCT; lst= constr_decl_list;             { Struct lst }
    | rh= sort_expr; "->"; lh= sort_expr;        { Function (rh, lh) }
    | rh= sort_expr; "#"; lh= sort_expr;         { Tuple (rh, lh) }

let sort_decl :=
    | lst= separated_nonempty_list(",", ID); ";";
                                                 { IdList lst }
    | id= ID; "="; signature= sort_expr; ";";    { SortType ( make_sort_type ~id
                                                                ~signature
                                                            )
                                                 }
let ids_decl :=
    | lst= separated_nonempty_list(",", ID); ":"; sort_expr= sort_expr;
                                                 { make_ids_decl ~id_list:lst ~sort_expr () }

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
    | specs= specs+; init= init?; EOF;           { Spec.make ~specs ?init ~loc:$loc () }
