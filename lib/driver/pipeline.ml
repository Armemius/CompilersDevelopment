open Common
open Lexer
open Parser

let compile (_source: string) =
  let lexer = Lexer.create _source in
  let tokens = Lexer.tokenize lexer in
  print_tokens tokens
