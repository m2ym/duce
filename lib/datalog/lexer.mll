{
  open Parser
}

rule token = parse
  | eof
      { EOF }
  | [' ' '\t' '\r' '\n']+
      { token lexbuf }
  | '#' [^ '\n']* '\n'
      { token lexbuf }

  | (['_' 'a'-'z' 'A'-'Z' '0'-'9']+) as symbol
      { SYMBOL symbol }
  | '\'' ([^ '\'']+? as symbol) '\''
      { QUOTED_CONST symbol }

  | ":-"    { LARROW }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '!'     { EXCLAM }
  | ','     { COMMA  }
  | '.'     { DOT    }

{
}
