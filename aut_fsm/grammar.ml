module State : sig
  type t [@@deriving show, eq]

  val make : int -> t
end = struct
  type t = int [@@deriving show, eq]

  let make i = i
end

module Edge : sig
  type t =
    { start_state: State.t
    ; label: string
    ; end_state: State.t
    }
  [@@deriving show, eq]

  val make : start_state:State.t -> label:string -> end_state:State.t -> t
end = struct
  type t =
    { start_state: State.t
    ; label: string
    ; end_state: State.t
    }
  [@@deriving show, eq, make]
end

module Header : sig
  type kind = Des

  type t =
    { kind: kind
    ; first_state: State.t
    ; nr_of_transistions: int
    ; nr_of_states: int
    }
  [@@deriving show, eq]

  val make :
       kind:kind
    -> first_state:State.t
    -> nr_of_transistions:int
    -> nr_of_states:int
    -> t
end = struct
  type kind = Des [@@deriving show, eq]

  type t =
    { kind: kind
    ; first_state: State.t
    ; nr_of_transistions: int
    ; nr_of_states: int
    }
  [@@deriving show, eq, make]
end

module Fsm : sig
  type t =
    { header: Header.t
    ; states: State.t list
    ; edges: Edge.t list
    }
  [@@deriving show, eq]

  val make : header:Header.t -> ?edges:Edge.t list -> unit -> t
end = struct
  type t =
    { header: Header.t
    ; states: State.t list
    ; edges: Edge.t list
    }
  [@@deriving show, eq]

  let make ~header ?(edges = []) () =
    { header
    ; states=
        List.fold_left
          (fun acc (e : Edge.t) ->
            let append_if_uniq itm lst =
              if List.mem itm lst then lst else itm :: lst
            in
            append_if_uniq e.start_state acc |> append_if_uniq e.end_state)
          [] edges
    ; edges
    }
end
