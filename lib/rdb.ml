open Batteries

type value = string

module Value = String
module ValueMap = Map.Make(Value)

type row = value array

module Row = struct
  type t = row

  let iter f t = Array.iter f t
  let iteri f t = Array.iteri f t
  let fold_left f t a = Array.fold_left f t a
  let fold_lefti f t a = Array.fold_left f t a
end

type index = row list ValueMap.t

module Index = ValueMap

type selection = int * value

type relation = {
  mutable r_arity    : int;
  mutable r_cardinal : int;
  mutable r_rows     : row list;
  mutable r_hashtbl  : (row, row) Hashtbl.t;
  mutable r_indices  : index array;
}

exception MissingRelation of string

module Relation = struct
  type t = relation

  let create =
    let dummy_indices = Array.make 0 Index.empty in
    fun () -> {
        r_arity    = 0;
        r_rows     = [];
        r_cardinal = 0;
        r_hashtbl  = Hashtbl.create 32;
        r_indices  = dummy_indices;
      }

  let init arity t =
    t.r_arity    <- arity;
    t.r_rows     <- [];
    t.r_cardinal <- 0;
    t.r_hashtbl  <- Hashtbl.create 32;
    t.r_indices  <- Array.make arity Index.empty

  let is_empty t = List.is_empty t.r_rows

  let arity t = t.r_arity

  let cardinal t = t.r_cardinal

  let mem row t = Hashtbl.mem t.r_hashtbl row

  let add row t =
    let arity = Array.length row in
    if t.r_arity = 0 then init arity t;
    t.r_rows <- row :: t.r_rows;
    t.r_cardinal <- t.r_cardinal + 1;
    Hashtbl.add t.r_hashtbl row row;
    Row.iteri
      (fun i v ->
         let index = t.r_indices.(i) in
         let block = try Index.find v index with Not_found -> [] in
         t.r_indices.(i) <- Index.add v (row :: block) index)
      row

  let select' conds t =
    if is_empty t
    then []
    else
      match conds with
      | [] ->
          t.r_rows
      | [(i,v,`Eq)] ->
          begin try Index.find v t.r_indices.(i) with Not_found -> [] end
      | [(i1,v1,`Eq); (i2,v2,`Eq)] ->
          (* TODO optimize *)
          let rows = try Index.find v1 t.r_indices.(i1) with Not_found -> [] in
          List.filter (fun row -> row.(i2) = v2) rows
      | _
        (* TODO *)
        -> failwith "Rdb.Relation.select'"

  let iter_select f conds t = List.iter f (select' conds t)

  let select conds t =
    let result = create () in
    iter_select (fun row -> add row result) conds t;
    result

  let fold f t a = List.fold_right f t.r_rows a

  let iter f t = List.iter f t.r_rows

  let iteri f t = List.iteri f t.r_rows

  let for_all f t = List.for_all f t.r_rows

  let to_list t = t.r_rows
end

module Pretty = struct
  let pp_value fmt value =
    Format.pp_print_string fmt value

  let pp_row fmt row =
    Format.fprintf fmt "[%a]"
      (Util.pp_list pp_value)
      (Array.to_list row)

  let pp_relation fmt rel =
    Relation.iteri
      (fun i row ->
         pp_row fmt row;
         Format.fprintf fmt "@\n")
      rel
end
