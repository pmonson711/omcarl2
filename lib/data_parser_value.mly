%start <Mcrl2.data_expr option> prog
%%

prog:
  | EOF       { None }
  | v = value { Some v }

value:
  | exp= data_expr;                                { exp }
