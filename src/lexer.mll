{
open Parser                          (* token variant *)

let kw = function
  | "end" -> END
  | "rec" -> REC
  | s     -> IDENT s
}

rule token = parse
  | [' ' '\t' '\r' '\n']             { token lexbuf }   (* skip blanks *)

  | "/*"                             { comment lexbuf } (* C-style comment *)

  | "->"                             { ARROW }
  | '?'                              { QUEST }
  | '!'                              { BANG  }
  | '{'                              { LBRACE }
  | '}'                              { RBRACE }
  | ':'                              { COLON  }
  | ','                              { COMMA  }
  | '|'                              { BAR    }
  | '.'                              { DOT    }

  | ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']* as id
                                      { kw id }

  | eof                              { EOF }

and comment = parse
  | "*/"                             { token lexbuf }
  | _                                { comment lexbuf }
