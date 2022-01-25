module Comment : sig
  type t =
    { text: string
    ; loc: Lexing.position * Lexing.position
    }
  [@@deriving show, eq, make]
end = struct
  type t =
    { text: string
    ; loc: Lexing.position * Lexing.position [@opaque] [@equal fun _ _ -> true]
    }
  [@@deriving show, eq, make]
end

type proj_decl =
  { proj_id: string option
  ; sort_expr: sort_expr
  }
[@@deriving show, eq]

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
  | List of sort_expr
  | Bag of sort_expr
  | FSet of sort_expr
  | FBag of sort_expr
  | Id of string
  | SubExpr of sort_expr
  | Struct of const_decl list
  | Function of sort_expr * sort_expr
  | Tuple of sort_expr * sort_expr
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
  | IdList of string list
  | SortType of sort_type
[@@deriving show, eq]

type mcrl2_spec_elt =
  | Comment       of Comment.t
  | SortSpec of sort_decl list
  | ConsSpec of ids_decl list
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
    ; loc: Lexing.position * Lexing.position
    }
  [@@deriving show, eq, make]
end = struct
  type t =
    { specs: mcrl2_spec_elt list
    ; init: init option
    ; loc: Lexing.position * Lexing.position [@opaque] [@equal fun _ _ -> true]
    }
  [@@deriving show, eq, make]
end
