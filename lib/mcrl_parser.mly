%start <Mcrl2.t list list option> prog
%%

prog:
  | EOF                                            { None }
  | sections= sections*; EOF                       { Some sections }

sections:
  | sorts=sort_spec;                               { sorts }
  | cons=cons_spec;                                { cons }
  | maps=map_spec;                                 { maps }
  | vars=var_spec;                                 { vars }
  | eqns=eqn_spec;                                 { eqns }
  | globs=glob_spec;                               { globs }
  | acts=act_spec;                                 { acts }

sort_spec:
  | SORT_SEC; sorts= sort_value+                   { sorts }

sort_value:
  | exp= sort_decl; ";"                            { `SortDecl exp }

sort_id_list:
  | lst= separated_nonempty_list(COMMA, ID)        { lst }

sort_decl:
  | lst= sort_id_list                              { `IdList lst }
  | id= ID; EQUAL; def= sort_exp                   { `Id (id, def) }

cons_spec:
  | CONS_SEC; cons= cons_value+                    { cons }

cons_value:
  | exp= cons_exp; SEMI_COlON                      { `ConsDecl exp }

map_spec:
  | MAP_SEC; maps= map_value+                      { maps }

map_value:
  | exp= map_exp; SEMI_COlON                       { `MapDecl exp }

var_spec:
  | VAR_SEC; vars= var_value*                      { vars }

var_value:
  | exp= var_exp; SEMI_COlON                       { `EqnDecl exp }

eqn_spec:
  | EQN_SEC; eqns= eqn_value*                      { eqns }

eqn_value:
  | exp= eqn_exp; SEMI_COlON                       { `EqnDecl exp }

glob_spec:
  | GLOB_SEC; globs= glob_value*                   { globs }

glob_value:
  | exp= glob_exp; SEMI_COlON                      { `GlobDecl exp }

act_spec:
  | ACT_SEC; acts= act_value*                      { acts }

act_value:
  | exp= act_exp; SEMI_COlON                       { `ActDecl exp }
