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
