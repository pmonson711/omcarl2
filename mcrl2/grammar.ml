type comment = { text: string } [@@deriving show, eq, make]

type proj_decl =
  { proj_id: string option
  ; sort_expr: sort_expr
  }
[@@deriving show, eq]
(** Sort Expressions *)

and const_decl =
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
  | Bag      of sort_expr
  | FSet     of sort_expr
  | FBag     of sort_expr
  | Id       of string
  | SubExpr  of sort_expr
  | Struct   of const_decl list
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
  | ForAll           of var_decl list * data_expr
  | Exists           of var_decl list * data_expr
  | Lambda           of var_decl list * data_expr
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
[@@deriving show, eq]

and bag_enum_elt = BagEnumElt of data_expr * data_expr

and var_decl = VarDecl of string * sort_expr

and assignment = Assignment of string * data_expr

type mcrl2_spec_elt =
  | Comment       of comment
  | SortSpec      of sort_decl list
  | ConsSpec      of ids_decl list
  | EqnSpec
  | GlobalVarSpec
  | ActSpec
  | ProcSpec
[@@deriving show, eq]

type init = ProcExpr [@@deriving show, eq]

module Spec : sig
  type t =
    { specs: mcrl2_spec_elt list
    ; init: init option
    }
  [@@deriving show, eq, make]
end = struct
  type t =
    { specs: mcrl2_spec_elt list
    ; init: init option
    }
  [@@deriving show, eq, make]
end
