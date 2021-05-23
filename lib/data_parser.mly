%%

%public data_expr:
  | id= ID                                         { `Id id }
  | num= NUMBER                                    { `Number num }
  | TRUE                                           { `True }
  | FALSE                                          { `False }
  | L_BRACE; R_BRACE                               { `List [] }
  | L_BRACK; R_BRACK                               { `Set [] }
  | L_BRACK; COLON; R_BRACK                        { `Bag [] }
  | L_BRACE; exprs= data_expr_list; R_BRACE        { `List exprs }
  | L_BRACK; exprs= bag_enum_elt_list; R_BRACK     { `Bag exprs }
  | "{"; v= var_decl; "|"; e= data_expr; "}"       { `SetComprehension (v, e) }
  | "{"; lst= data_expr_list; "}"                  { `Set lst }
  | "("; exp= data_expr; ")"                       { exp }

data_expr_list:
  | lst= separated_nonempty_list(COMMA, data_expr) { lst }

bag_enum_elt:
  | exp1= data_expr; COLON; exp2= data_expr        { `BagEnumElt (exp1, exp2) }

bag_enum_elt_list:
  | lst= separated_nonempty_list(COMMA, bag_enum_elt)
                                                   { lst }

(* data_id_list: *)
(*   | lst= separated_nonempty_list(COMMA, ID)        { lst } *)

var_decl:
  | id= ID; COLON; expr= sort_exp;                 { `VarDecl (id, expr) }

(* vars_decl: *)
(*   | lst=  data_id_list; COLON; expr= sort_exp;     { `VarDecl (id, expr) } *)

(* vars_decl_list: *)
(*   | lst= separated_nonempty_list(COMMA, vars_decl) { lst } *)

(* assignment: *)
(*   | id= ID; EQUAL; expr= data_expr;                { `Assignment (id, expr) } *)

(* assignment_list: *)
(*   | lst= separated_nonempty_list(COMMA, assignment) { lst } *)

