open Lexer.Token
open Ast
include Ast

type parser_state = {
  tokens : t array;
  mutable pos : int;
}

exception Parse_error of string

let create_state tokens = { tokens = Array.of_list tokens; pos = 0 }

let peek st =
  if st.pos >= Array.length st.tokens then EOF
  else st.tokens.(st.pos)

let next st =
  let t = peek st in
  if st.pos < Array.length st.tokens then
    st.pos <- st.pos + 1;
  t

let parse_error st message =
  let token = peek st in
  let full_message =
    Printf.sprintf "Parse error at token %d (%s): %s" st.pos (to_string token) message
  in
  raise (Parse_error full_message)

let expect st expected message =
  match peek st with
  | tok when tok = expected -> ignore (next st)
  | _ -> parse_error st message

let consume_identifier st =
  match peek st with
  | ID name ->
      ignore (next st);
      name
  | _ -> parse_error st "Expected identifier"

let rec parse_primary st =
  match peek st with
  | NUMBER n ->
      ignore (next st);
      Int (int_of_string n)
  | ID name ->
      ignore (next st);
      Var name
  | LPAREN ->
      ignore (next st);
      let expr = parse_expression st in
      expect st RPAREN "Expected ')' after expression";
      expr
  | _ -> parse_error st "Expected expression"

and parse_unary st =
  match peek st with
  | MINUS ->
      ignore (next st);
      let right = parse_unary st in
      Binop (Sub, Int 0, right)
  | _ -> parse_primary st

and parse_factor st =
  let rec loop left =
    match peek st with
    | STAR ->
        ignore (next st);
        let right = parse_unary st in
        loop (Binop (Mul, left, right))
    | SLASH ->
        ignore (next st);
        let right = parse_unary st in
        loop (Binop (Div, left, right))
    | _ -> left
  in
  let left = parse_unary st in
  loop left

and parse_term st =
  let rec loop left =
    match peek st with
    | PLUS ->
        ignore (next st);
        let right = parse_factor st in
        loop (Binop (Add, left, right))
    | MINUS ->
        ignore (next st);
        let right = parse_factor st in
        loop (Binop (Sub, left, right))
    | _ -> left
  in
  let left = parse_factor st in
  loop left

and parse_comparison st =
  let rec loop left =
    match peek st with
    | GREATER ->
        ignore (next st);
        let right = parse_term st in
        loop (Binop (Greater, left, right))
    | GEQ ->
        ignore (next st);
        let right = parse_term st in
        loop (Binop (Geq, left, right))
    | LESS ->
        ignore (next st);
        let right = parse_term st in
        loop (Binop (Less, left, right))
    | LEQ ->
        ignore (next st);
        let right = parse_term st in
        loop (Binop (Leq, left, right))
    | _ -> left
  in
  let left = parse_term st in
  loop left

and parse_equality st =
  let rec loop left =
    match peek st with
    | EQEQ ->
        ignore (next st);
        let right = parse_comparison st in
        loop (Binop (Eq, left, right))
    | NEQ ->
        ignore (next st);
        let right = parse_comparison st in
        loop (Binop (Neq, left, right))
    | _ -> left
  in
  let left = parse_comparison st in
  loop left

and parse_expression st = parse_equality st

let rec parse_statement st =
  match peek st with
  | VAR -> parse_var_decl st
  | PRINT -> parse_print st
  | IF -> parse_if st
  | WHILE -> parse_while st
  | ID _ -> parse_assignment st
  | _ -> parse_error st "Expected statement"

and parse_var_decl st =
  expect st VAR "Expected 'var'";
  let name = consume_identifier st in
  expect st EQ "Expected '=' after variable name";
  let value = parse_expression st in
  expect st SEMICOLON "Expected ';' after variable declaration";
  VarDecl (name, value)

and parse_assignment st =
  let name = consume_identifier st in
  expect st EQ "Expected '=' after identifier";
  let value = parse_expression st in
  expect st SEMICOLON "Expected ';' after assignment";
  Assign (name, value)

and parse_print st =
  expect st PRINT "Expected 'print'";
  let value = parse_expression st in
  expect st SEMICOLON "Expected ';' after print statement";
  Print value

and parse_if st =
  expect st IF "Expected 'if'";
  expect st LPAREN "Expected '(' after 'if'";
  let condition = parse_expression st in
  expect st RPAREN "Expected ')' after if condition";
  let then_branch = parse_block_or_statement st in
  let else_branch =
    match peek st with
    | ELSE ->
        ignore (next st);
        Some (parse_block_or_statement st)
    | _ -> None
  in
  If (condition, then_branch, else_branch)

and parse_while st =
  expect st WHILE "Expected 'while'";
  expect st LPAREN "Expected '(' after 'while'";
  let condition = parse_expression st in
  expect st RPAREN "Expected ')' after while condition";
  let body = parse_block_or_statement st in
  While (condition, body)

and parse_block_or_statement st =
  match peek st with
  | LBRACE -> parse_block st
  | _ -> [parse_statement st]

and parse_block st =
  expect st LBRACE "Expected '{' to start block";
  let rec loop acc =
    match peek st with
    | RBRACE ->
        ignore (next st);
        List.rev acc
    | EOF -> parse_error st "Unterminated block"
    | _ ->
        let statement = parse_statement st in
        loop (statement :: acc)
  in
  loop []

let parse tokens =
  let st = create_state tokens in
  let rec parse_program acc =
    match peek st with
    | EOF -> List.rev acc
    | _ ->
        let statement = parse_statement st in
        parse_program (statement :: acc)
  in
  parse_program []

let parse_expr tokens =
  let st = create_state tokens in
  let expr = parse_expression st in
  begin
    match peek st with
    | EOF -> ()
    | _ -> parse_error st "Expected end of expression"
  end;
  expr
