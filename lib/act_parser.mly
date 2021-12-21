%%

%public act_exp:
  | ids= act_id_list; COLON; expr=sort_exp  { `DataAct (ids, expr) }
  | ids= act_id_list;                       { `Act (ids) }

act_id_list:
  | lst= separated_nonempty_list(COMMA, ID) { lst }
