%%

%public proc_decl:
  | id= ID; "="; expr= proc_expr          { `Proc (id, [], [expr]) }
  | id= ID; "("; lst= vars_decl_list; ")"; "="; expr= proc_expr
                                          { `Proc (id, lst, [expr]) }

proc_expr:
  | c= condition                          { c }
  | e= proc_expr_no_if                    { e }
  | "("; e= proc_expr_no_if; ")"          { e }

proc_expr_no_if:
  | id= ID                                { `Action id }
  | id= ID; "("; ")";                     { `Action id }
  | id= ID; "("; lst= data_expr_list; ")" { `DataAction (id, lst) }
  | DELTA                                 { `Delta }
  | TAU                                   { `Tau }
  | l= proc_expr; op= proc_bin_op; r= proc_expr
                                          { `BinOp (l, op, r) }
  | "sum"; lst= vars_decl_list; "."; expr= proc_expr
                                          { `Summation (lst, expr) }

condition:
  | "{"; c= data_expr_unit; ")"; "->"; expr= proc_expr
                                          { `Condition (c, expr) }
  | "{"; c= data_expr_unit; ")"; "->"; e1= proc_expr; "<>"; e2= proc_expr
                                          { `ConditionElse (c, e1, e2) }

proc_bin_op:
  | "."                                   { `SequentialComposition }
  | "+"                                   { `AlternativeComposition }
  | "||"                                  { `ParallelComposition }
  | "|"                                   { `SimultaneousComposition }

data_expr_unit:
  | data= data_expr;                      { `DataExpr data }
