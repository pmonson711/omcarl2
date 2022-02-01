type comment = { text: string } [@@deriving show, eq, make]

type proj_decl =
  { proj_id: string option
  ; sort_expr: sort_expr
  }
[@@deriving show, eq]
(** Sort Expressions *)

and constr_decl =
  { const_id: string
  ; proj_decls: proj_decl list
  ; guard: string option
  }
[@@deriving show, eq]

and sort_expr =
  | Bool
  | Pos
  | Nat
  | Int
  | Real
  | List     of sort_expr
  | Set      of sort_expr
  | Bag      of sort_expr
  | FSet     of sort_expr
  | FBag     of sort_expr
  | Id       of string
  | SubExpr  of sort_expr
  | Struct   of constr_decl list
  | Function of sort_expr * sort_expr
  | Tuple    of sort_expr * sort_expr
[@@deriving show, eq]

type ids_decl =
  { id_list: string list
  ; sort_expr: sort_expr
  }
[@@deriving show, eq, make]

type sort_type =
  { id: string
  ; signature: sort_expr
  }
[@@deriving show, eq, make]

type sort_decl =
  | IdList   of string list
  | SortType of sort_type
[@@deriving show, eq]

type data_expr =
  | Id               of string
  | Number           of int
  | Bool             of bool
  | List             of data_expr list
  | Bag              of bag_enum_elt list
  | SetComp          of var_decl * data_expr
  | Set              of data_expr list
  | SubExpr          of data_expr
  | SetUpdate        of data_expr * data_expr * data_expr
  | Access           of data_expr * data_expr list
  | Neg              of data_expr
  | Invert           of data_expr
  | Count            of data_expr
  | ForAll           of vars_decl list * data_expr
  | Exists           of vars_decl list * data_expr
  | Lambda           of vars_decl list * data_expr
  | Implies          of data_expr * data_expr
  | Or               of data_expr * data_expr
  | And              of data_expr * data_expr
  | Equal            of data_expr * data_expr
  | NotEqual         of data_expr * data_expr
  | LessThan         of data_expr * data_expr
  | LessThanEqual    of data_expr * data_expr
  | GreaterThanEqual of data_expr * data_expr
  | GreaterThan      of data_expr * data_expr
  | In               of data_expr * data_expr
  | Snoc             of data_expr * data_expr
  | Cons             of data_expr * data_expr
  | Concat           of data_expr * data_expr
  | Sum              of data_expr * data_expr
  | Difference       of data_expr * data_expr
  | Quotient         of data_expr * data_expr
  | IntDivision      of data_expr * data_expr
  | Remainder        of data_expr * data_expr
  | Product          of data_expr * data_expr
  | AtPosition       of data_expr * data_expr
  | Where            of data_expr * assignment list
[@@deriving show, eq]

and bag_enum_elt = BagEnumElt of data_expr * data_expr

and var_decl = VarDecl of string * sort_expr [@@deriving show, eq, make]

and vars_decl = VarsDecl of string list * sort_expr
[@@deriving show, eq, make]

and assignment = Assignment of string * data_expr

type eqn_decl = EqnDecl of data_expr option * data_expr option * data_expr
[@@deriving show, eq]

type var_spec = VarSpec of vars_decl list list [@@deriving show, eq]

type act_decl =
  | IdList      of string list
  | SortProduct of string list * sort_expr
[@@deriving show, eq]

type action =
  { id: string
  ; data_expr_list: data_expr list
  }
[@@deriving show, eq, make]

type proc_expr =
  | Action of action
  | Call   of string * assignment list
  | Delta
  | Tau
  | Block  of string list * proc_expr
[@@deriving show, eq]

type proc_decl = ProcDelc of string * vars_decl list * proc_expr
[@@deriving show, eq]

type mcrl2_spec_elt =
  | Comment       of comment
  | SortSpec      of sort_decl list
  | ConsSpec      of ids_decl list
  | MapSpec       of ids_decl list
  | EqnSpec       of var_spec option * eqn_decl list
  | GlobalVarSpec of vars_decl list list
  | ActSpec       of act_decl list
  | ProcSpec      of proc_decl list
[@@deriving show, eq]

module Spec : sig
  type t =
    { specs: mcrl2_spec_elt list
    ; init: proc_expr option
    }
  [@@deriving show, eq, make]
end = struct
  type t =
    { specs: mcrl2_spec_elt list
    ; init: proc_expr option
    }
  [@@deriving show, eq, make]
end
