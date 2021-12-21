%start <Mcrl2.t option> prog
%%

prog:
  | EOF       { None }
  | v = value { Some v }

value:
  | exp= glob_exp; SEMI_COlON { `GlobDecl exp }
