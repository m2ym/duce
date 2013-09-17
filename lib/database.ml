open Batteries
open Datalog

type db_engine = Ast.program -> Ast.atom -> Rdb.relation

type database = {
  db_program : Ast.program;
  db_engine  : db_engine
}

let create program engine = {
  db_program = Alpha.run program;
  db_engine  = engine;
}

let query t query =
  let q_vars    = Ast.Query.vars' query in
  let q_vars    = Array.map (fun v -> Ast.Var v) (Array.of_list q_vars) in
  let q_atom    = Ast.Atom ("?", q_vars) in
  let q_rule    = Ast.Rule (q_atom, query) in
  let l_program = q_rule :: t.db_program in
  t.db_engine l_program q_atom
