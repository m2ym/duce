%{
  open Batteries
  open Ast
%}

%token <string> SYMBOL
%token LARROW LPAREN RPAREN EXCLAM COMMA DOT
%token EOF

%start program query rule
%type <Ast.rule list> program
%type <Ast.query>     query
%type <Ast.rule>      rule

%%

program:
    EOF                         { [] }
  | rules EOF                   { $1 }

query:
    literals DOT                { $1 }

rule:
    rule1                       { $1 }
  | EOF                         { raise End_of_file }

rule1:
    atom DOT                    { Rule ($1, []) }
  | atom LARROW literals DOT    { Rule ($1, $3) }

rules:
  | rule1                        { [$1] }
  | rule1 rules                  { $1 :: $2 }

literal:
    atom                        { Positive $1 }
  | EXCLAM atom                 { Negative $2 }

literals:
  |                             { [] }
  | literal                     { [$1] }
  | literal COMMA literals      { $1 :: $3 }

atom: 
    SYMBOL LPAREN terms RPAREN  { Atom ($1, Array.of_list $3) }

term:
    SYMBOL                      { let s = $1 in
                                  if Char.is_uppercase s.[0] || s.[0] = '_'
                                  then Var s
                                  else Const s }

terms:
  |                             { [] }
  | term                        { [$1] }
  | term COMMA terms            { $1 :: $3 }
