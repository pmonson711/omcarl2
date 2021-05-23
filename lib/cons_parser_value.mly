%start <Mcrl2.t option> prog
%%

prog:
  | EOF       { None }
  | v = value { Some v }

value:
  | exp= cons_exp; SEMI_COlON                      { `ConsDecl exp }

