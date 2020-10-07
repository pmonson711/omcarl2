type sort_type_op =
  | Lambda
  | Tuple

and sort_exp =
  [ `Boolean
  | `Positive
  | `Natural
  | `Integer
  | `Real
  | `List      of sort_exp
  | `Set       of sort_exp
  | `Bag       of sort_exp
  | `FiniteSet of sort_exp
  | `FiniteBag of sort_exp
  | `Ref       of string
  | `Struct    of constr_decl list
  | `TypeOp    of sort_type_op * sort_exp * sort_exp
  ]

and proj_decl =
  [ `TDecl of string * sort_exp
  | `Decl  of string
  ]

and sort_decl =
  [ `IdList of string list
  | `Id     of string * sort_exp
  ]

and constr_decl =
  [ `TConstr of string * proj_decl list * string option
  | `Constr  of string * string option
  ]

type t = [ `SortDecl of sort_decl ]
