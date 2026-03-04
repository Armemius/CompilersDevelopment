open Lexer
open Parser

let build_ast source =
  let lexer = Lexer.create source in
  let tokens = Lexer.tokenize lexer in
  Parser.parse tokens

let compile source =
  ignore (build_ast source)
