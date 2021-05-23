%start <Mcrl2.t option> prog
%%

prog:
  | EOF       { None }
  | v = value { Some v }

sort_id_list:
  | lst= separated_nonempty_list(COMMA, ID)        { lst }

sort_decl:
  | lst= sort_id_list                              { `IdList lst }
  | id= ID; EQUAL; def= sort_exp                   { `Id (id, def) }

value:
  | exp= sort_decl; SEMI_COlON                     { `SortDecl exp }
