%%

%public eqn_exp:
  | e1=data_expr; "="; rewrite= data_expr   { `Eqn (None, e1, rewrite) }
  | c=data_expr; "->"; e1=data_expr; "="; rewrite= data_expr
                                            { `Eqn (Some c, e1, rewrite) }

%public var_exp:
  | ids= eqn_id_list; COLON; expr=sort_exp  { `Var (ids, expr) }

eqn_id_list:
  | lst= separated_nonempty_list(COMMA, ID) { lst}
