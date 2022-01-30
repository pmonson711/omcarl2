%start <Mcrl2.t option> prog
%%

prog:
  | EOF       { None }
  | v = value { Some v }

value:
  | exp= proc_decl; SEMI_COlON { `ProcDecl exp }
