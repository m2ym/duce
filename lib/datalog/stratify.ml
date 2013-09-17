open Batteries
open Ast

module PG =
  Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
    (struct
      include String
      let hash = Hashtbl.hash
    end)
    (struct
      include Bool
      let hash = to_int
      let default = true
    end)

let precedence_graph program =
  let edb = Program.edb program in
  let make_edge concl hypo =
    (Atom.pred concl,
     Literal.is_positive hypo || Set.mem (Literal.pred hypo) edb,
     Literal.pred hypo)
  in
  let edges =
    List.concat
      (List.map
         (fun (Rule (concl, hypos)) ->
            List.map (make_edge concl) hypos)
         program)
  in List.fold_right (flip PG.add_edge_e) edges PG.empty

let is_stratifiable pg =
  let module Components = Graph.Components.Make(PG) in
  List.for_all
    (fun scc ->
       List.for_all
         (fun v1 ->
            List.for_all
              (fun v2 ->
                 try
                   let (_,mode,_) = PG.find_edge pg v1 v2 in mode
                 with Not_found -> true)
              scc)
         scc)
    (Components.scc_list pg)

let run program =
  let pg = precedence_graph program in
  if is_stratifiable pg
  then
    let module Topological = Graph.Topological.Make(PG) in
    let preds_list =
      Topological.fold
        (fun pred acc ->
           if List.exists (fun (_,positive,_) -> not positive) (PG.pred_e pg pred)
           then [pred] :: acc
           else (pred :: List.hd acc) :: List.tl acc)
        pg
        [[]]
    in
    let result =
      List.map
        (fun preds -> List.concat (List.map (flip (Program.find_rules) program) preds))
        preds_list
    in Some (List.filter (fun s -> not (List.is_empty s)) result)
  else None
