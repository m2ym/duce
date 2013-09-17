open Format
open Ast
open Util

let rec pp_program fmt program =
  pp_seq pp_rule fmt program

and pp_rule fmt = function
  | Rule (concl, []) ->
      fprintf fmt "%a." pp_atom concl
  | Rule (concl, hypos) ->
      fprintf fmt "%a :- %a."
        pp_atom concl
        (pp_list pp_literal) hypos

and pp_literal fmt = function
  | Positive atom -> pp_atom fmt atom
  | Negative atom -> fprintf fmt "!%a" pp_atom atom

and pp_atom fmt (Atom (pred, args)) =
  fprintf fmt "%s(%a)" pred (pp_array pp_term) args

and pp_term fmt = function
  | Var s   -> pp_print_string fmt s
  | Const s -> pp_print_string fmt s

let pp_print_program fmt program = pp_program fmt program
let pp_print_rule fmt rule = pp_rule fmt rule
let print_program program = pp_print_program std_formatter program
let print_rule rule = pp_print_rule std_formatter rule
