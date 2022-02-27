%start <Mcrl2.t option> prog
%%

prog:
  | EOF       { None }
  | v = value { Some v }

value:
  | exp= act_exp; SEMI_COlON { `ActDecl exp }
