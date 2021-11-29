module Parameter : sig
  type t [@@deriving show, eq]

  val make :
       parameter_name:string
    -> domain_cardinality:int
    -> domain_name:string
    -> ?values:string list
    -> unit
    -> t
end = struct
  type t =
    { parameter_name: string
    ; domain_cardinality: int
    ; domain_name: string
    ; values: string list
    }
  [@@deriving make, show, eq]
end

module State : sig
  type t [@@deriving show, eq]

  val make : int list -> unit -> t
end = struct
  type t = int list [@@deriving show, eq]

  let make lst () = lst
end

module Transistion : sig
  type t [@@deriving show, eq]

  val make : source_state:int -> target_state:int -> label:string -> t
end = struct
  type t =
    { source_state: int
    ; target_state: int
    ; label: string
    }
  [@@deriving make, show, eq]
end

module Fsm : sig
  type t [@@deriving show, eq]

  val default : t

  val make :
       ?parameters:Parameter.t list
    -> ?states:State.t list
    -> ?transistions:Transistion.t list
    -> unit
    -> t
end = struct
  type t =
    { parameters: Parameter.t list
    ; states: State.t list
    ; transistions: Transistion.t list
    }
  [@@deriving make, show, eq]

  let default = make ()
end
