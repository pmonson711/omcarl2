%%

%public glob_exp:
  | ids= glob_id_list; COLON; expr=sort_exp  { `Glob (ids, expr) }

glob_id_list:
  | lst= separated_nonempty_list(COMMA, ID) { lst }
