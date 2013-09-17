open Batteries
open Ast

let remove_tautology program =
  List.filter_map
    (function
      | Rule (concl, [Positive hypo]) when concl = hypo -> None
      | r -> Some r)
    program

let run program =
  remove_tautology program
