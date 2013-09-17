open Batteries
open Ducelib
open Datalog

let () =
  let program = Parse.parse_from_file Sys.argv.(1) in
  let db      = Database.create program (Topdown.Eval.eval Map.empty) in
  let lexbuf  = Lexing.from_channel stdin in
  while true do
    Format.printf "> %!";
    let query    = Parser.query Lexer.token lexbuf in
    let relation = Database.query db query in
    Format.printf "%a" Rdb.Pretty.pp_relation relation
  done
