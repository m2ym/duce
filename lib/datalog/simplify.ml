open Batteries
open Ast

let run program =
  let rec work acc = function
    | [] -> List.rev acc
    | Rule (c, hs) as r :: rs ->
        begin match hs with
          | h1 :: h2 :: (_ :: _ as hs) ->
              let c_vars = Atom.vars c in
              let hs_vars =
                List.fold_right
                  Set.union
                  (List.map Literal.vars hs)
                  Set.empty
              in
              let chs_vars = Set.union c_vars hs_vars in
              let h1_vars =
                Set.intersect
                  (Literal.vars h1)
                  (Set.union (Literal.vars h2) chs_vars)
              in
              let h2_vars =
                Set.intersect
                  (Literal.vars h2)
                  (Set.union (Literal.vars h1) chs_vars)
              in
              let vars = Set.union h1_vars h2_vars in
              let temp_concl = Atom (Util.gensym (), Array.map (fun s -> Var s) (Array.of_enum (Set.enum vars))) in
              let temp_hypos = [h1; h2] in
              let temp_rule = Rule (temp_concl, temp_hypos) in
              let new_rule = Rule (c, Positive temp_concl :: hs) in
              work (temp_rule :: acc) (new_rule :: rs)
          | _ ->
              work (r :: acc) rs
        end
  in work [] program
