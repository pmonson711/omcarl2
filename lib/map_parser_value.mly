%start <Mcrl2.t option> prog
%%

prog:
  | EOF       { None }
  | v = value { Some v }

value:
  | exp= map_exp; SEMI_COlON { `MapDecl exp }
