open Batteries
open Ast

let alpha rename args =
  Array.map
    (function
      | Const _ as arg -> arg
      | Var "_" ->
          Var (Util.gensym ())
      | Var n ->
          try Var (Map.find n !rename)
          with Not_found ->
            let n' = Util.gensym () in
            rename := Map.add n n' !rename;
            Var n')
    args

let convert_atom rename (Atom (pred, args)) =
  Atom (pred, alpha rename args)

let convert_rule (Rule (concl, hypos)) =
  let rename = ref Map.empty in
  let concl  = convert_atom rename concl in
  let hypos  =
    List.map
      (function
        | Positive hypo -> Positive (convert_atom rename hypo)
        | Negative hypo -> Negative (convert_atom rename hypo))
      hypos
  in Rule (concl, hypos)

let run program = List.map convert_rule program
