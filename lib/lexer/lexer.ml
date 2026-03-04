open Token

module Token = Token

type t = {
  input : string;
  length : int;
  mutable pos : int;
}

let create input = { input; length = String.length input; pos = 0 }

let peek lexer =
  if lexer.pos >= lexer.length then '\000'
  else lexer.input.[lexer.pos]

let next lexer =
  if lexer.pos >= lexer.length then '\000'
  else
    let c = lexer.input.[lexer.pos] in
    lexer.pos <- lexer.pos + 1;
    c

let is_letter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
let is_digit c = '0' <= c && c <= '9'
let is_whitespace c =
  c = ' ' || c = '\t' || c = '\n' || c = '\r'

let tokenize_number lexer =
  let start = lexer.pos in
  while is_digit (peek lexer) do
    ignore (next lexer)
  done;
  let value = String.sub lexer.input start (lexer.pos - start) in
  NUMBER value

let tokenize_word lexer =
  let start = lexer.pos in
  while is_letter (peek lexer) || is_digit (peek lexer) do
    ignore (next lexer)
  done;
  let word = String.sub lexer.input start (lexer.pos - start) in
  match word with
  | "var" -> VAR
  | "print" -> PRINT
  | "if" -> IF
  | "else" -> ELSE
  | "while" -> WHILE
  | _ -> ID word

let tokenize_operator lexer =
  let c = next lexer in
  match c with
  | '+' -> PLUS
  | '-' -> MINUS
  | '*' -> STAR
  | '/' -> SLASH
  | '=' -> 
      if peek lexer = '=' then (ignore (next lexer); EQEQ) else EQ
  | '!' ->
      if peek lexer = '=' then (ignore (next lexer); NEQ)
      else failwith (Printf.sprintf "Unexpected character '!' at position %d" lexer.pos)
  | '<' ->
      if peek lexer = '=' then (ignore (next lexer); LEQ) else LESS
  | '>' ->
      if peek lexer = '=' then (ignore (next lexer); GEQ) else GREATER
  | ';' -> SEMICOLON
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '{' -> LBRACE
  | '}' -> RBRACE
  | '\000' -> EOF
  | _ -> failwith (Printf.sprintf "Unexpected character '%c' at position %d" c lexer.pos)

let rec tokenize lexer =
  if lexer.pos >= lexer.length then [EOF]
  else
    let c = peek lexer in
    if is_whitespace c then (
      ignore (next lexer);
      tokenize lexer
    ) else if is_digit c then
      let tok = tokenize_number lexer in
      tok :: tokenize lexer
    else if is_letter c then
      let tok = tokenize_word lexer in
      tok :: tokenize lexer
    else
      let tok = tokenize_operator lexer in
      tok :: tokenize lexer

let print_tokens (tokens: Token.t list) =
  List.iter (fun tok ->
    print_endline (Token.to_string tok)
  ) tokens
