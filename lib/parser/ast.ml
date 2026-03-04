type expr =
  | Int of int
  | Var of string
  | Binop of binop * expr * expr

and binop =
  | Add | Sub | Mul | Div
  | Greater | Geq | Less | Leq
  | Eq | Neq

type stmt =
  | VarDecl of string * expr
  | Assign of string * expr
  | Print of expr
  | If of expr * stmt list * stmt list option
  | While of expr * stmt list

type program = stmt list

type tree = Node of string * tree list

let string_of_binop = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Greater -> "Greater"
  | Geq -> "Geq"
  | Less -> "Less"
  | Leq -> "Leq"
  | Eq -> "Eq"
  | Neq -> "Neq"

let rec expr_to_tree = function
  | Int value -> Node (Printf.sprintf "Int(%d)" value, [])
  | Var name -> Node (Printf.sprintf "Var(%s)" name, [])
  | Binop (op, left, right) ->
      Node
        ( Printf.sprintf "Binop(%s)" (string_of_binop op),
          [Node ("left", [expr_to_tree left]); Node ("right", [expr_to_tree right])] )

let rec stmt_to_tree = function
  | VarDecl (name, value) ->
      Node (Printf.sprintf "VarDecl(%s)" name, [Node ("value", [expr_to_tree value])])
  | Assign (name, value) ->
      Node (Printf.sprintf "Assign(%s)" name, [Node ("value", [expr_to_tree value])])
  | Print value ->
      Node ("Print", [Node ("value", [expr_to_tree value])])
  | If (condition, then_branch, else_branch) ->
      let else_nodes =
        match else_branch with
        | None -> []
        | Some stmts -> [Node ("else", List.map stmt_to_tree stmts)]
      in
      Node
        ( "If",
          [Node ("condition", [expr_to_tree condition]); Node ("then", List.map stmt_to_tree then_branch)]
          @ else_nodes )
  | While (condition, body) ->
      Node
        ( "While",
          [Node ("condition", [expr_to_tree condition]); Node ("body", List.map stmt_to_tree body)] )

let program_to_tree (program : program) =
  Node ("Program", List.map stmt_to_tree program)

let render_tree node =
  let buffer = Buffer.create 256 in

  let rec render_child prefix is_last (Node (label, children)) =
    Buffer.add_string buffer prefix;
    Buffer.add_string buffer (if is_last then "\\- " else "|- ");
    Buffer.add_string buffer label;
    Buffer.add_char buffer '\n';
    let next_prefix = prefix ^ if is_last then "   " else "|  " in
    render_children next_prefix children

  and render_children prefix children =
    match children with
    | [] -> ()
    | [child] -> render_child prefix true child
    | child :: rest ->
        render_child prefix false child;
        render_children prefix rest
  in

  let Node (root, children) = node in
  Buffer.add_string buffer root;
  Buffer.add_char buffer '\n';
  render_children "" children;
  Buffer.contents buffer

let string_of_expr expr =
  render_tree (expr_to_tree expr)

let string_of_stmt stmt =
  render_tree (stmt_to_tree stmt)

let string_of_program program =
  render_tree (program_to_tree program)

let pp_expr fmt expr =
  Format.pp_print_string fmt (string_of_expr expr)

let pp_stmt fmt stmt =
  Format.pp_print_string fmt (string_of_stmt stmt)

let pp_program fmt program =
  Format.pp_print_string fmt (string_of_program program)
