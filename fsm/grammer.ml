type parameter =
  { parameter_name: string
  ; domain_cardinality: int
  ; domain_name: string
  ; values: string list
  } [@@deriving make, show, eq]

type state = int list [@@deriving show, eq]

type transistion = 
  { source_state: int
  ; target_state: int
  ; label: string
  } [@@deriving make, show, eq]

type t =
  { parameters: parameter list
  ; states: state list
  ; transistions: transistion list
  } [@@deriving make, show, eq]
