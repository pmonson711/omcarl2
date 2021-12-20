%start <Mcrl2.t option> prog
%%

prog:
  | EOF       { None }
  | v = value { Some v }

value:
  | exp= var_exp; SEMI_COlON { `EqnDecl exp }
  | exp= eqn_exp; SEMI_COlON { `EqnDecl exp }
