type t =
  (* Keywords *)
  | VAR | PRINT | IF | ELSE | WHILE

  (* Literals *)
  | ID of string
  | NUMBER of string

  (* Arithmetic Operators *)
  | PLUS | MINUS | STAR | SLASH | EQ
  
  (* Comparison operators *)
  | GREATER | GEQ | LESS | LEQ | EQEQ | NEQ
  
  (* Punctuation *)
  | SEMICOLON
  | LPAREN | RPAREN
  | LBRACE | RBRACE

  (* End of input *)
  | EOF

let to_string = function
  | VAR -> "VAR"
  | PRINT -> "PRINT"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | WHILE -> "WHILE"
  | ID s -> "ID(" ^ s ^ ")"
  | NUMBER n -> "N(" ^ n ^ ")"
  | PLUS -> "+"
  | MINUS -> "-"
  | STAR -> "*"
  | SLASH -> "/"
  | GREATER -> ">"
  | GEQ -> ">="
  | LESS -> "<"
  | LEQ -> "<="
  | EQ -> "="
  | EQEQ -> "=="
  | NEQ -> "!="
  | SEMICOLON -> ";"
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACE -> "{"
  | RBRACE -> "}"
  | EOF -> "EOF"
