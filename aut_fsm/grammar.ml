module State : sig
  type t [@@deriving show, eq]

  val make : int -> unit -> t
end = struct
  type t = int [@@deriving show, eq]

  let make i () = i
end

module Transition : sig
  type t [@@deriving show, eq]

  val make : source_state:int -> target_state:int -> label:string -> t
end = struct
  type t =
    { source_state: int
    ; target_state: int
    ; label: string
    }
  [@@deriving show, eq, make]
end

module Header : sig
  type kind = Des

  type t [@@deriving show, eq]

  val make :
       kind:kind
    -> first_state:State.t
    -> nr_of_transistions:int
    -> nr_of_states:int
    -> t
end = struct
  type kind = | Des [@@deriving show, eq]

  type t =
    { kind: kind
    ; first_state: State.t
    ; nr_of_transistions: int
    ; nr_of_states: int
    }
  [@@deriving show, eq, make]
end

module Fsm : sig
  type t [@@deriving show, eq]

  val make :
       header:Header.t
    -> ?states:State.t list
    -> ?transistions:Transition.t list
    -> unit
    -> t
end = struct
  type t =
    { header: Header.t
    ; states: State.t list
    ; transistions: Transition.t list
    }
  [@@deriving show, eq, make]
end
