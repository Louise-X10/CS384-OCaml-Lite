open Ast
open Typechecker

exception InterpError of string

(* name to value binding *)
type env = (string * value) list

  and value = 
    | VInt of int | VString of string | VBool of bool | VUnit
    (* function arg * environment * func expr * optional func name for recursion *)
    | VClosure of string * env * expr * string option 
    (* constructor name * function that takes arguments and returns value *)
    | VConstructor of string * (value list -> value)
    (* constructor name * arguments *)
    | VUser of string * value list

module Interpretor = struct 
  type interp_state = {env: env}
  let empty_state = {env = []}
  module InterpState = State(struct type state = interp_state end)
  open InterpState

let rec valueof_expr (e:expr) :value = 
  let st = interp_expr e in 
  let ret_v, _ = run_state st empty_state in 
  ret_v

and interp_expr: expr -> value InterpState.m = function
  | CInt i -> return (VInt i)
  | CString s -> return (VString s) 
  | CBool b -> return (VBool b)
  | Unit -> return VUnit
  | Binop (e1, op, e2) -> interp_binop e1 op e2
  | Unop (op, e) -> interp_unop op e
  (* | Tuple elst -> interp_tuple elst
  | IfExp(e1, e2, e3) -> interp_ifexp e1 e2 e3
  | LetExp(x, b, plst, t, e1, e2) -> interp_letexp x b plst t e1 e2
  | MatchExp(e, mlst) -> interp_matchexp e mlst
  | App(e1, e2) -> interp_app e1 e2
  | Function(plst, t, e) -> interp_function plst t e *)
  (* For variable, look in context first, if not there then invalid *)
  | Var s -> 
    (
      match s with 
      | _ -> get >>= fun st ->
        let v_opt = List.assoc_opt s st.env in 
        let ret_v = 
        (match v_opt with | Some v -> v | None -> raise (InterpError ("Can't find variable " ^ s ^ " in environment"))) in 
        return ret_v
    )
  | _ -> return VUnit

and interp_binop (e1: expr) (op: binop) (e2: expr): value InterpState.m = 
  let v1 = eval_state (interp_expr e1) empty_state in 
  let v2 = eval_state (interp_expr e2) empty_state  in 
  match (v1, v2) with 
  | VInt i1, VInt i2 -> 
    (match op with 
    | Add -> return (VInt (i1 + i2))
    | Sub -> return (VInt (i1 - i2))
    | Mul -> return (VInt (i1 * i2))
    | Div -> return (VInt (i1 / i2))
    | Modulo -> return (VInt (i1 mod i2))
    | LessThan -> return (VBool (i1 < i2))
    | Equal -> return (VBool (i1 = i2))
    | _ -> raise (InterpError "Unexpected binop arg type" )
    )
  | VString s1, VString s2 -> 
    (match op with 
    | StrConcat -> return (VString (s1 ^ s2))
    | Equal -> return (VBool (s1 = s2))
    | _ -> raise (InterpError "Unexpected binop arg type" )
    )
  | VBool b1, VBool b2 -> 
    (match op with 
    | LogicAnd -> return (VBool (b1 && b2))
    | LogicOr -> return (VBool (b1 || b2))
    | Equal -> return (VBool (b1 = b2))
    | _ -> raise (InterpError "Unexpected binop arg type" )
    )
  | _ -> raise (InterpError "Unexpected binop arg type" )

and interp_unop (op: unop) (e: expr): value InterpState.m = 
  let v = eval_state (interp_expr e) empty_state in 
  match v with 
  | VBool b -> 
    (match op with 
    | LogicNegate ->return (VBool (not b))
    | _ -> raise (InterpError "Unexpected unop arg type" )
    )
  | VInt i -> 
    (match op with 
    | IntNegate -> return (VInt (-1 * i))
    | _ -> raise (InterpError "Unexpected unop arg type" )
    )
  | _ -> raise (InterpError "Unexpected unop arg type" )

and interp_tuple(elst: expr list) : value = 
  failwith "undefined"
and interp_ifexp (e1: expr) (e2: expr) (e3: expr): value = 
  failwith "undefined"

and interp_app (e1: expr) (e2: expr): value = 
  failwith "undefined"

and interp_function (plst: param list) (t: typ option) (e: expr) : value = 
  failwith "undefined"
and interp_matchexp (e: expr) (brlst: matchbranch list): value = 
  failwith "undefined"
and interp_letexp (x: string) (b:bool) (plst: param list) (t:typ option) (e1: expr) (e2:expr): value = 
  failwith "undefined"

let rec interp_binding: binding -> value * env = function
| LetB(x, b, plst, t, e1) -> interp_letbinding x b plst t e1
| TypeB(x, tblst) -> interp_typebinding x tblst

and interp_letbinding (x: string) (b:bool) (plst: param list) (t:typ option) (e1: expr): value * env = 
  failwith "undefined"

(* Return unit value, add typ constructors to env *)
and interp_typebinding (s: string) (tblst: typ_binding list): value * env = 
  failwith "undefined"

end

let interp_expr (e: expr) : value = 
  Interpretor.valueof_expr e