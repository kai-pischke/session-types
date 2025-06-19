%{
open Ast
%}

%token <string> IDENT
%token END REC
%token ARROW QUEST BANG
%token LBRACE RBRACE COLON COMMA BAR DOT
%token LPAREN RPAREN
%token EOF

/* Entry points that must hit end-of-file */
%start <string Ast.global> gfile
%start <string Ast.local > lfile
%%

ident:
  IDENT                                   { $1 }

/* ───── global types ───── */
gfile:
  global_type EOF                         { $1 }

global_type:
  END                                     { GEnd Loc.dummy }
| ident                                   { GVar ($1, Loc.dummy) }
| REC ident DOT global_type               { GRec ($2, $4, Loc.dummy) }
| ident ARROW ident LBRACE g_lab RBRACE   { GBra ($1, $3, $5, Loc.dummy) }
| global_type BAR global_type             { GPar ($1, $3, Loc.dummy) }
| LPAREN global_type RPAREN               { $2 }

g_lab:
  separated_nonempty_list(COMMA, g_pair)  { $1 }

g_pair:
  ident COLON global_type                 { ($1, $3) }

/* ───── local types ───── */
lfile:
  local_type EOF                          { $1 }

local_type:
  END                                     { LEnd Loc.dummy }
| ident                                   { LVar ($1, Loc.dummy) }
| REC ident DOT local_type                { LRec ($2, $4, Loc.dummy) }
| ident QUEST LBRACE l_lab RBRACE         { LRecv ($1, $4, Loc.dummy) }
| ident BANG  LBRACE l_lab RBRACE         { LSend ($1, $4, Loc.dummy) }
| LPAREN local_type RPAREN                { $2 }

l_lab:
  separated_nonempty_list(COMMA, l_pair)  { $1 }

l_pair:
  ident COLON local_type                  { ($1, $3) }
