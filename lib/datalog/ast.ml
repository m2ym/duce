open Batteries

type program = rule list

and query = literal list

and rule = Rule of atom * literal list

and literal = Positive of atom | Negative of atom

and atom = Atom of string * term array

and term = Var of string | Const of string

module Term = struct
  type t = term

  let is_var = function
    | Var _ -> true
    | _     -> false

  let is_const = function
    | Const _ -> true
    | _       -> false
end

module Atom = struct
  type t = atom

  let pred (Atom (pred, _)) = pred
  let args (Atom (_, args)) = args
  let arity t = Array.length (args t)

  let vars' (Atom (_, args)) =
    List.unique
      (List.filter_map
         (function
           | Var "_" -> None
           | Var s   -> Some s
           | _       -> None)
         (Array.to_list args))

  let vars t = Set.of_list (vars' t)
end

module Literal = struct
  type t = literal

  let is_positive = function
    | Positive _ -> true
    | _          -> false

  let is_negative = function
    | Negative _ -> true
    | _          -> false

  let atom = function
    | Positive atom -> atom
    | Negative atom -> atom

  let pred t = Atom.pred (atom t)
  let args t = Atom.args (atom t)
  let arity t = Atom.arity (atom t)
  let vars t = Atom.vars (atom t)
  let vars' t = Atom.vars' (atom t)
end

module Rule = struct
  type t = rule

  let concl (Rule (concl, _)) = concl
  let hypos (Rule (_, hypos)) = hypos
  let atoms (Rule (c, hs))    = c :: List.map Literal.atom hs

  let pred t  = Atom.pred (concl t)
  let arity t = Atom.arity (concl t)
end

module Query = struct
  type t = query

  let vars' t =
    List.unique
      (List.fold_right
         (fun q vs -> Literal.vars' q @ vs)
         t [])

  let vars t = Set.of_list (vars' t)
end

module Program = struct
  type t = program

  let rules t = List.filter (fun r -> not (List.is_empty (Rule.hypos r))) t
  let facts t = List.filter (fun r -> List.is_empty (Rule.hypos r)) t
  let find_rules pred t =
    List.filter
      (function
        | Rule (Atom (p, _), _) -> p = pred)
      t

  let atoms t = List.concat (List.map (fun r -> List.rev (Rule.atoms r)) t)
  let arity t =
    let table =
      List.fold_right
        (fun a m -> Map.add (Atom.pred a) (Atom.arity a) m)
        (atoms t)
        Map.empty
    in fun pred -> Map.find pred table

  let sch  t = Set.of_list (List.map Atom.pred (atoms t))
  let sch' t = List.unique (List.map Atom.pred (atoms t))
  let idb  t = Set.of_list (List.map Rule.pred t)
  let idb' t = List.unique (List.map Rule.pred t)
  let edb  t = Set.diff (sch t) (idb t)
end
