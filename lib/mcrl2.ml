type sort_type_op =
  | Lambda
  | Tuple
[@@deriving show, eq]

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
[@@deriving show, eq]

and proj_decl =
  [ `TDecl of string * sort_exp
  | `Decl  of string
  ]
[@@deriving show, eq]

and sort_decl =
  [ `IdList of string list
  | `Id     of string * sort_exp
  ]
[@@deriving show, eq]

and constr_decl =
  [ `TConstr of string * proj_decl list * string option
  | `Constr  of string * string option
  ]
[@@deriving show, eq]

and cons_decl = [ `IdList of string list * sort_exp option ]
[@@deriving show, eq]

and data_expr =
  [ `Id               of string
  | `Number           of int
  | `True
  | `False
  | `List             of data_expr list
  | `Set              of data_expr list
  | `Bag              of bag_enum_elt list
  | `SetComprehension of var_decl * data_expr
  | `SetUpdate        of data_expr * data_expr * data_expr
  | `FunctionApply    of data_expr * data_expr list
  | `SetCompliment    of data_expr
  | `Negation         of data_expr
  | `Length           of data_expr
  | `ForAll           of var_decl list * data_expr
  | `Exists           of var_decl list * data_expr
  | `Lambda           of var_decl list * data_expr
  | `BinOp            of data_expr * data_bin_op * data_expr
  | `WhereOp          of data_expr * assignment list
  ]
[@@deriving show, eq]

and data_bin_op =
  [ `Cons
  | `Equality
  | `GreaterThan
  | `GreaterThanEqual
  | `In
  | `Inequality
  | `LessThan
  | `LessThanEqual
  | `LogicalAnd
  | `LogicalImplication
  | `LogicalOr
  | `Snoc
  | `ListConcat
  | `Sum
  | `Difference
  | `Product
  | `Quotient
  | `IntegerDivision
  | `Remainder
  | `AtPosition
  ]
[@@deriving show, eq]

and assignment = [ `Assignment of string * data_expr ] [@@deriving show, eq]

and var_decl =
  [ `VarDecl  of string * sort_exp
  | `VarsDecl of string list * sort_exp
  ]
[@@deriving show, eq]

and bag_enum_elt = [ `BagEnumElt of data_expr * data_expr ]
[@@deriving show, eq]

and map_decl = [ `IdsDecl of string list * sort_exp ] [@@deriving show, eq]

and eqn_decl =
  [ `Var of string list * sort_exp
  | `Eqn of data_expr option * data_expr * data_expr
  ]
[@@deriving show, eq]

and glob_decl = [ `Glob of string list * sort_exp ] [@@deriving show, eq]

and act_decl =
  [ `DataAct of string list * sort_exp
  | `Act of string list
] [@@deriving show, eq]

type t =
  [ `SortDecl of sort_decl
  | `ConsDecl of cons_decl
  | `MapDecl  of map_decl
  | `EqnDecl  of eqn_decl
  | `GlobDecl of glob_decl
  | `ActDecl  of act_decl
  ]
[@@deriving show, eq]
