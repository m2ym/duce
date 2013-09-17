open Batteries
open Datalog
open Ast
open Rdb

module ModuloAtom = struct
  include Atom

  let hash (Atom (pred, args)) =
    let c = ref 1 in
    let m = ref Map.empty in
    Array.fold_left
      (fun h -> function
         | Const v -> h * 13 + Hashtbl.hash v
         | Var n   ->
             try Map.find n !m
             with Not_found ->
               let a = !c in
               incr c;
               m := Map.add n a !m;
               h * 13 + a)
      (Hashtbl.hash pred) args

  let equal (Atom (pred1, args1)) (Atom (pred2, args2)) =
    (pred1 = pred2) && (Array.length args1 = Array.length args2) &&
    (let m = ref Map.empty in
     Array.for_all2
       (fun x y -> match x, y with
          | Const v1, Const v2 -> v1 = v2
          | Var n1, Var n2 ->
              (try Map.find n1 !m = n2
               with Not_found ->
                 m := Map.add n1 n2 !m;
                 true)
          | _, _ -> false)
       args1 args2)
end

module Table = struct
  module HT = Hashtbl.Make(ModuloAtom)

  type t = (atom * relation) HT.t

  let create () = HT.create 32

  let add t k v = HT.add t k (k, v)

  let replace t k v = HT.add t k (k, v)

  let find_binding t k = HT.find t k

  let find t k = snd (find_binding t k)
end

let terms_of_row row = Array.map (fun v -> Const v) row

let row_of_terms terms =
  Array.map
    (function
      | Var n   -> failwith ("Unbound variable: " ^ n)
      | Const v -> v)
    terms

let rec deref t s = match t with
  | Const _ -> t
  | Var n   ->
      try
        (match Map.find n s with
         (* TODO path compression *)
         | Var m as t -> deref t s
         | t          -> t)
      with Not_found -> t

let unify x y =
  if Array.length x <> Array.length y
  then None
  else
    let rec loop i l x y s =
      if i >= l
      then Some s
      else
        match deref x.(i) s, deref y.(i) s with
        | Var n, (Var m as t) ->
            loop (i+1) l x y
              (if n = m then s else Map.add n t s)
        | Var n, t
        | t, Var n ->
            loop (i+1) l x y (Map.add n t s)
        | Const v1, Const v2 ->
            if v1 = v2
            then loop (i+1) l x y s
            else None
    in loop 0 (Array.length x) x y Map.empty

let subst x s = Array.map (flip deref s) x

let eval instance =
  let suspension = Hashtbl.create 32 in
  let table = Table.create () in
  fun program ->
    let idb = Program.idb program in
    let rec call q r i s =
      if i < List.length (Rule.hypos r)
      then begin
        match List.nth (Rule.hypos r) i with
        | Negative _ -> failwith "Not implemented yet for negative hypothesis"
        | Positive (Atom (pred, args)) ->
            let args = subst args s in
            let h    = Atom (pred, args) in
            if not (Set.mem pred idb)
            then
              Relation.iter
                (fun row -> match unify args (terms_of_row row) with
                   | None    -> ()
                   | Some s' -> call q r (i+1) (Map.union s s'))
                (try Map.find pred instance
                 with Not_found -> raise (MissingRelation pred))
            else
              try
                let (k, rel) = Table.find_binding table h in
                Hashtbl.add suspension k (h, q, r, s, i);
                Relation.iter
                  (fun row -> match unify args (terms_of_row row) with
                     | None    -> failwith "Something wrong"
                     | Some s' -> call q r (i+1) (Map.union s s'))
                  rel
              with Not_found ->
                Table.add table h (Relation.create ());
                Hashtbl.add suspension h (h, q, r, s, i);
                List.iter
                  (fun r' ->
                     let r' = Alpha.convert_rule r' in
                     match unify (Atom.args (Rule.concl r')) args with
                     | None    -> ()
                     | Some s' -> call h r' 0 s')
                  (Program.find_rules pred program)
      end else begin
        let args = subst (Atom.args q) s in
        let row  = row_of_terms args in
        let rel  = Table.find table q in
        if not (Relation.mem row rel) then begin
          Relation.add row rel;
          List.iter
            (fun (h, q, r, s, i) ->
               match unify (Atom.args h) args with
               | None    -> failwith "Something wrong"
               | Some s' -> call q r (i+1) (Map.union s s'))
            (Hashtbl.find_all suspension q)
        end
      end;
    in
    fun q_atom ->
      Table.replace table q_atom (Relation.create ());
      List.iter
        (fun r ->
           let r = Alpha.convert_rule r in
           match unify (Atom.args (Rule.concl r)) (Atom.args q_atom) with
           | None   -> ()
           | Some s -> call q_atom r 0 s)
        (Program.find_rules (Atom.pred q_atom) program);
      Table.find table q_atom
