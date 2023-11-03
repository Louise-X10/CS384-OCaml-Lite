type binding = 
  | LetB of string * param list * typ option * expr (* let $id [<param>]* [: <type>] = <expr> *)
  | LetRecB of string * param list * typ option * expr (* let rec $id [<param>]* [: <type>] = <expr> *)
  | TypeB of string * typ_binding list (* type $id = ['|' $id [of <type>]]+ *)

and typ_binding = string * typ option

and expr =
  (* Binary operations *)
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Modulo of expr * expr
  | LessThan of expr * expr
  | Equal of expr * expr
  | LogicAnd of expr * expr
  | LogicOr of expr * expr
  | StrConcat of expr * expr
  (* Unary operations *)
  | LogicNegate of expr
  | IntNegate of expr
  (* Base expressions *)
  | CInt of int
  | CString of string
  | CBool of bool
  | Var of string
  (* Complex expressions *)
  | MatchExp of expr * matchbranch list (* match <expr> with ['|' <match_branch>]+ *)
  | LetExp of string * param list * typ option * expr * expr (* let $id [<param>]* [: <type>] = <expr> in <expr> *)
  | LetRecExp of string * param list * typ option * expr * expr (* let rec $id [<param>]* [: <type>] = <expr> in <expr> *)
  | IfExp of expr * expr * expr (* if <expr> then <expr> else <expr> *)
  | Function of param list * typ option * expr (* fun [<param>]+ [: <type>] => <expr> , type is return type*)
  | App of expr * expr (* <expr> <expr> *)
  | Tuple of expr list (* ( <expr> [, <expr>]+ ) *)
  | Unit

and matchbranch = 
  | MatchBr of string * pattern_vars option  * expr (* <match_branch> ::= $id [<pattern_vars>] => <expr> *)

and pattern_vars = string list (* <pattern_vars> ::= $id| ( $id [, $id ]+) *)

and param = {
  name: string;
  p_type: typ option;
}

and typ = 
  | IntTy 
  | BoolTy 
  | StringTy
  | UnitTy 
  | FuncTy of typ * typ 
  | TupleTy of typ list
  | CustomTy of string

type program = binding list