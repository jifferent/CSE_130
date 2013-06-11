{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

rule token = parse
    eof         { EOF }
  | [' ' '\t' '\n' '\r']    { token lexbuf }
  
 // | "hd"     { HD }
 // | "tl"     { TL }
  
  | "::"     { COLONCOLON }
  | ";"      { SEMI }
  | "]"      { RBRAC }
  | "["      { LBRAC }
  
  | ")"      { RPAREN }
  | "("      { LPAREN }
  
  | "||"      { OR }
  | "&&"      { AND }
  | "!="      { NE }
  | "<="      { LE }
  | "<"       { LT }
  | "/"       { DIV }
  | "*"       { MUL }
  | "-"       { MINUS }
  | "+"       { PLUS }
  
  | "else"    { ELSE }
  | "then"    { THEN }
  | "if"      { IF }
  | "->"      { ARROW }
  | "fun"     { FUN }
  | "in"      { IN }
  | "="       { EQ }
  | "rec"     { REC }
  | "let"     { LET }
  
  | "false"   { False }
  | "true"    { True }

  | ['0'-'9']+ as n { Num (int_of_string n) }
  | ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9']* as x { Id x }
  | _           { raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }
