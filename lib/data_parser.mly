%%

%public data_expr:
  | expr= static                                   { expr }
  | expr= sets                                     { expr }
  | expr= lists                                    { expr }
  | expr= bags                                     { expr }
  | "("; expr= data_expr; ")"                      { expr }
  | expr= set_update                               { expr }
  | expr= func_apply                               { expr }
  | "!"; expr= data_expr                           { `SetCompliment expr }
  | "-"; expr= data_expr                           { `Negation expr }
  | "#"; expr= data_expr                           { `Length expr }
  | "forall"; vars= vars_decl_list; "."; expr= data_expr
                                                   { `ForAll (vars, expr) }
  | "exists"; vars= vars_decl_list; "."; expr= data_expr
                                                   { `Exists (vars, expr) }
  | "lambda"; vars= vars_decl_list; "."; expr= data_expr
                                                   { `Lambda (vars, expr) }
  | expr= bin                                      { expr }

bin:
  | left= data_expr; op= bin_op; right= data_expr  { `BinOp (left, op, right) }
  | expr= data_expr; WHERE; lst= assignment_list; END
                                                   { `WhereOp (expr, lst) }

bin_op:
  | "=>"                                           { `LogicalImplication }
  | "||"                                           { `LogicalOr }
  | "&&"                                           { `LogicalAnd }
  | "=="                                           { `Equality }
  | "!="                                           { `Equality }
  | "<"                                            { `LessThan }
  | "<="                                           { `LessThanEqual }
  | ">"                                            { `GreaterThan }
  | ">="                                           { `GreaterThanEqual }
  | "|>"                                           { `Cons }
  | "<|"                                           { `Snoc }
  | "in"                                           { `In }
  | "++"                                           { `ListConcat }
  | "+"                                            { `Sum }
  | "-"                                            { `Difference }
  | "*"                                            { `Product }
  | "/"                                            { `Quotient }
  | "div"                                          { `IntegerDivision }
  | "mod"                                          { `Remainder }
  | "."                                            { `AtPosition }

sets:
  | "{"; v= var_decl; "|"; e= data_expr; "}"       { `SetComprehension (v, e) }
  | "{"; lst= data_expr_list; "}"                  { `Set lst }
  | L_BRACK; R_BRACK                               { `Set [] }

bags:
  | L_BRACK; COLON; R_BRACK                        { `Bag [] }
  | L_BRACK; exprs= bag_enum_elt_list; R_BRACK     { `Bag exprs }

lists:
  | L_BRACE; exprs= data_expr_list; R_BRACE        { `List exprs }
  | L_BRACE; R_BRACE                               { `List [] }

static:
  | id= ID; EOF?                                   { `Id id }
  | num= NUMBER                                    { `Number num }
  | TRUE                                           { `True }
  | FALSE                                          { `False }

data_expr_list:
  | lst= separated_nonempty_list(COMMA, data_expr) { lst }

bag_enum_elt:
  | exp1= data_expr; COLON; exp2= data_expr        { `BagEnumElt (exp1, exp2) }

bag_enum_elt_list:
  | lst= separated_nonempty_list(COMMA, bag_enum_elt)
                                                   { lst }
data_id_list:
  | lst= separated_nonempty_list(COMMA, ID)        { lst }

var_decl:
  | id= ID; COLON; expr= sort_exp;                 { `VarDecl (id, expr) }

vars_decl:
  | lst=  data_id_list; COLON; expr= sort_exp;     { `VarsDecl (lst, expr) }

vars_decl_list:
  | lst= separated_nonempty_list(COMMA, vars_decl) { lst }

assignment:
  | id= ID; EQUAL; expr= data_expr;                { `Assignment (id, expr) }

assignment_list:
  | lst= separated_nonempty_list(COMMA, assignment) { lst }

set_update:
  | exp1= ID; "["; exp2= data_expr; "->"; exp3= data_expr "]"
                                                   { `SetUpdate (`Id exp1, exp2, exp3) }

func_apply:
  | expr1= ID; "("; lst= data_expr_list; ")"       { `FunctionApply (`Id expr1, lst) }
