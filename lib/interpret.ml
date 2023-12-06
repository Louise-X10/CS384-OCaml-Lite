open Ast
open Typechecker

exception InterpError of string

(* name to value binding *)
type env = (string * value) list
and value = 
  | VInt of int | VString of string | VBool of bool | VUnit
  | VTuple of value list
  (* deferred function: function arg * environment * func expr * optional func name for recursion *)
  | VClosure of string * env * expr * string option 
  (* user-defined type constructor: constructor name * function that takes argument value(s) and returns value *)
  | VConstructor of string * (value -> value)
  (* user-defined constructed value: constructor name * arguments *)
  | VUser of string * value list
  | VBuiltin of string

module Interpretor = struct 
  type interp_state = {env: env}
  let empty_st = {env = []}
  module InterpState = State(struct type state = interp_state end)
  open InterpState

(* Helper: chain multiple interp env in order 
     Return list of interp values and current env *)
let rec chain_interp_states (st_lst: value InterpState.m list) (init_st: interp_state): value list * interp_state = 
  match st_lst with 
  | [] -> failwith "Error in chaining binding states"
  | [st] -> 
    let b_val, b_st = run_state st init_st in 
    [b_val], b_st
  | st :: tail -> 
    let b_val, b_st = run_state st init_st in 
    let b_vals, ret_st = chain_interp_states tail b_st in 
    b_val:: b_vals, ret_st

(* Get list of free variables in given expr *)
let rec valueof_expr (e:expr) :value = 
  let st = interp_expr e in 
  let ret_v = eval_state st empty_st in 
  ret_v

(* and valueof_binding (b:binding) : value = 
  let st = interp_binding b in 
  let ret_v = eval_state st empty_st in 
  ret_v *)

and retst_program (blst:program) :interp_state = 
  let st_lst = List.map interp_binding blst in 
  let _, ret_env = chain_states st_lst empty_st in 
  ret_env

and interp_expr: expr -> value InterpState.m = function
  | CInt i -> return (VInt i)
  | CString s -> return (VString s) 
  | CBool b -> return (VBool b)
  | Unit -> return VUnit
  | Binop (e1, op, e2) -> interp_binop e1 op e2
  | Unop (op, e) -> interp_unop op e
  | Tuple elst -> interp_tuple elst
  | IfExp(e1, e2, e3) -> interp_ifexp e1 e2 e3
  | App(e1, e2) -> interp_app e1 e2
  | Function(plst, t, e) -> interp_function plst t e None 
  | MatchExp(e, mlst) -> interp_matchexp e mlst
  | LetExp(x, b, plst, t, e1, e2) -> interp_letexp x b plst t e1 e2
  (* For variable, look in context first, if not there then invalid *)
  | Var s -> 
    (match s with 
    | "int_of_string" -> return (VBuiltin ("int_of_string"))
    | "string_of_int" -> return (VBuiltin ("string_of_int"))
    | "print_string" -> return (VBuiltin ("print_string"))
    | _ -> get >>= fun initial_st ->
      let v_opt = List.assoc_opt s initial_st.env in 
      let ret_v = 
      (match v_opt with | Some v -> v | None -> raise (InterpError ("Can't find variable " ^ s ^ " in environment"))) in 
      return ret_v
    )

(* Helper: get value of expression with given initial state *)
and value_fromst (e:expr) (state: interp_state) :value = 
  let st = interp_expr e in 
  let ret_v = eval_state st state in 
  ret_v

and add_closure_env (ret_closure: value) (arg_name: string) (arg_v: value): value = 
  match ret_closure with 
  | VClosure(arg, env, exp, name_opt) ->  VClosure(arg, (arg_name, arg_v) :: env, exp, name_opt) 
  | _ -> raise (InterpError "Not a closure, cannot add new arg-val binding to closure env")

(* Evaluate both e1 e2 in initial state, return resulting value *)
and interp_binop (e1: expr) (op: binop) (e2: expr): value InterpState.m = 
  get >>= fun initial_st ->
  let v1 = eval_state (interp_expr e1) initial_st in 
  let v2 = eval_state (interp_expr e2) initial_st  in 
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

(* Evaluate e in initial state, return negated value *)
and interp_unop (op: unop) (e: expr): value InterpState.m = 
  get >>= fun initial_st ->
  let v = eval_state (interp_expr e) initial_st in 
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

(* Evaluate each e in initial state, return VTuple *)
and interp_tuple(elst: expr list) : value InterpState.m = 
  get >>= fun initial_st ->
    let helper (e: expr) = value_fromst e initial_st in 
  let vlst = List.map helper elst in 
  return (VTuple vlst)

(* Evaluate each condition e, if true then return v2, else return v3 *)
and interp_ifexp (e1: expr) (e2: expr) (e3: expr): value InterpState.m = 
  get >>= fun initial_st ->
    let v1 = value_fromst e1 initial_st in 
    match v1 with 
    | VBool true -> return (value_fromst e2 initial_st)
    | VBool false -> return (value_fromst e3 initial_st)
    | _ -> raise (InterpError "If condition is not a boolean")

(* Evaluate e, find branch with matching type constructor. 
   Assign argument values in e to names in branch pvars (if there are any), then evaluate branch expr *)
and interp_matchexp (e: expr) (brlst: matchbranch list): value InterpState.m = 
  get >>= fun initial_st ->
    let v = value_fromst e initial_st in 
    (* Find matching branch *)
    let extract_vuser (v: value) = 
      match v with 
      | VUser(constructor, arg_vlst) -> constructor, arg_vlst
      | _ -> raise (InterpError "Match expression is not user-defined type") in 
    let c, arg_vlst = extract_vuser v in 
    let find_matchbr (br: matchbranch): bool = 
      let MatchBr(s, _, _) = br in 
      if s = c then true else false in 
    let br = List.find find_matchbr brlst in 
    (* Assign arg_vlst to pvars, add to interp_state, evaluate br_e *)
    (* ! Add pvar bindings to front of env to shadow any previous assignments *)
    match br with 
    | MatchBr(_, Some pvars, br_e) -> 
      let branch_st = {env = (List.combine pvars arg_vlst) @ initial_st.env} in 
      let ret_v = value_fromst br_e branch_st in 
      return ret_v
    | MatchBr(_, None, br_e) -> 
      return (value_fromst br_e initial_st)

(* Evaluate e1 e2 in initial state. 
    If v1 is a type constructor, apply v2 as args, return v. 
    If v1 is closure, apply v2 as single arg, either return resulting v or another closure. 
    If v1 is recursive closure, add closure_name to env, apply v2 as arg.
*)
and interp_app (e1: expr) (e2: expr): value InterpState.m = 
  get >>= fun initial_st ->
  let v1 = value_fromst e1 initial_st in 
  let v2 = value_fromst e2 initial_st in 
  let ret_v = 
  (match v1 with 
    (* If v1 is constructor, apply v2 (can be tuple) as args to constructor *)
    | VConstructor(_, constructor_func) -> constructor_func v2
    (* If v1 is buildin function, directly apply v2 *)
    | VBuiltin("int_of_string") -> 
      let (VString s) = v2 in VInt (int_of_string s)
    | VBuiltin("string_of_int") -> 
      let (VInt i) = v2 in VString (string_of_int i)
    | VBuiltin("print_string") -> 
      let (VString s) = v2 in 
      let _ = print_endline s in VUnit
    (* If v1 is function, then evaluate closure body with binding arg=v2 *)
    | VClosure(arg, closure_env, closure_exp, None) -> 
      (match closure_exp with
      (* If closure body is a function, interp it within closure env to get another closure, add arg-v2 binding to closure env *)
      | Function(_, _, _) -> 
        let ret_closure = value_fromst closure_exp {env=closure_env} in 
        let ret_closure = add_closure_env ret_closure arg v2 in 
        ret_closure
      (* Otherwise, add arg-v2 binding to closure env, then evaluate closure body along with initial state env *)
      | _ -> 
        value_fromst closure_exp {env = (arg, v2):: closure_env @ initial_st.env}
      )
    (* If v1 is recursive function, then add closure_name=closure, arg-v2 to state env, then evaluate closure exp *)
    | VClosure(arg, closure_env, closure_exp, Some closure_name) -> 
      let rec_closure_st = {env=(arg, v2)::(closure_name, VClosure(arg, closure_env, closure_exp, Some closure_name))::closure_env @ initial_st.env} in 
      value_fromst closure_exp rec_closure_st
    | _ -> raise (InterpError "Expression is not function or constructor, cannot be applied")
  ) in 
  return ret_v

(* Evaluate function and return closure. 
    For non-recursive function, return closure with name of single arg, env, function expr 
    For recursive function, return closure with name of single arg, env, function expr, and function mame
*)
and interp_function (plst: param list) (t: typ option) (e: expr) (name_opt: string option) : value InterpState.m = 
  get >>= fun initial_st ->
    (* Extract first arg into closure *)
    match plst with 
    | [arg] -> return (VClosure (arg.name, initial_st.env, e, name_opt))
    | arg :: args -> return (VClosure (arg.name, initial_st.env, Function(args, t, e), name_opt))
    | [] -> raise (InterpError "Function has no parameters")

(* Get env with let binding, then evaluate e2 in updated env
   Note that returned state is not modified
*)
and interp_letexp (x: string) (b:bool) (plst: param list) (t:typ option) (e1: expr) (e2:expr): value InterpState.m = 
  get >>= fun initial_st -> 
  let _, let_st = run_state (interp_letbinding x b plst t e1) initial_st in
  let v2 = value_fromst e2 let_st in 
  return v2

and interp_binding: binding -> value InterpState.m = function
| LetB(x, b, plst, t, e1) -> interp_letbinding x b plst t e1
| TypeB(x, tblst) -> interp_typebinding x tblst

(* Add letbinding to env, return unit value *)
and interp_letbinding (x: string) (b:bool) (plst: param list) (t:typ option) (e1: expr): value InterpState.m = 
  get >>= fun initial_st -> 
    (* If recursive, generate rec func name *)
    (* Assume that cannot have recursive let with no plst *)
    let closure_name: string option = if b = true then Some x else None in 
    let v1 = 
      (* If there is plst, interp like a function *)
      if List.length plst > 0 then eval_state (interp_function plst t e1 closure_name) initial_st
      (* If no plst, interpret like an expression *)
      else eval_state (interp_expr e1) initial_st in 
  put {env = (x, v1) :: initial_st.env} >>= fun _ -> 
    return VUnit

(* Add typ constructors to env, return unit value *)
and interp_typebinding (_: string) (tblst: typ_binding list): value InterpState.m = 
  (* Get constructor - VConstructor value binding *)
  let get_typ_constructor_val (tb: typ_binding): string * value = 
    let constructor, args_opt = tb in 
    match args_opt with 
    | None -> constructor, VUser(constructor, [])
    (* Note: Doesn't check that args has correct type, that is done by typechecker or when it's evaluated by interp *)
    | Some _ -> 
      let tuple_to_lst (v: value): value list = 
        (match v with 
          | VTuple vlst -> vlst
          | v -> [v]
        ) in 
      constructor, 
      (* When (tuple of) args are supplied to VConstructor, return VUser with constructor name and list of args *)
    VConstructor(constructor, fun (args: value) -> VUser(constructor, tuple_to_lst args)) in 
  let bindings = List.map get_typ_constructor_val tblst in 
  get >>= fun st -> 
  put {env = st.env @ bindings} >>= fun _ ->
    return VUnit 
end

let interp_expr (e: expr) : value = 
  Interpretor.valueof_expr e

let interp_program (e:program) : Interpretor.interp_state = 
  Interpretor.retst_program e

let interpret (e:program) : unit = 
  let _ = Interpretor.retst_program e in ()