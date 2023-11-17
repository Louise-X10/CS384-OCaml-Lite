open Ast

exception TypeError of string

module type MONAD = sig
  type 'a m
  val return : 'a -> 'a m
  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m

  (* Implicitly, we expect monads to satisfy three conditions:
     - return x >>= f is the same as f x
     - m >>= return is the same as m
     - (m >>= f) >>= g is the same as m >>= (fun x -> f x >>= g) *)
end

(* The State monad for general stateful computations. *)

module State (S : sig type state end) : sig
  include MONAD
  val get : S.state m
  val put : S.state -> unit m
  val run_state : 'a m -> S.state -> 'a * S.state
  val eval_state : 'a m -> S.state -> 'a
  val exec_state : 'a m -> S.state -> S.state
end = struct
  open S
  type 'a m = state -> 'a * state  (* given current state -> return value * modified resulting state *)
  let return (x : 'a) : state -> 'a * state = fun st -> (x, st)
  let ( >>= ) (x : state -> 'a * state)
      (f : 'a -> state -> 'b * state) : state -> 'b * state =
    fun st -> let (v, st2) = x st in f v st2
  let get : state m = fun st -> (st, st)
  let put (st : state) : unit m = fun _ -> ((), st)
  let run_state (x : 'a m) (st : state) : 'a * state = x st
  let eval_state (x : 'a m) (st : state) : 'a = fst (x st)
  let exec_state (x : 'a m) (st : state) : state = snd (x st)
end

module TypeChecker = struct
  type constr = typ * typ (* e.g. (t0, int) *)
  type context = string * typ (* e.g. (x, t0) *)
  type constr_state = {clst: constr list; context: context list}
  module ConstrState = State(struct type state = constr_state end)
  open ConstrState

  (* Generate new type variables *)
  let next_var = ref 0
  let fresh_var (_ : unit) : typ =
    let () = next_var := !next_var + 1 in
    CustomTy ("t$" ^ string_of_int !next_var)

  (* Helper: Merge multiple context lists without duplicates *)
  let merge_context (lst_of_context: context list list)  = 
    (* let comp = (fun (x, _) (y, _) -> String.compare x y) in *)
    let contexts = List.fold_left (@) [] lst_of_context in 
    List.sort_uniq compare contexts

  (* Helper: Merge multiple constraint lists without duplicates *)
  let merge_constr (lst_of_constr: constr list list)  = 
    (* let comp = (fun (x, x') (y, y') -> String.compare x y) in *)
    let constraints = List.fold_left (@) [] lst_of_constr in 
    List.sort_uniq compare constraints

  (* Helper: Get initial state from param list. 
    See (x: int), return {clst = [(t0, int)]; context = [(x, t0)]}
    See (x), return {clst = []; context = [(x, t0)]}
  *)
  let rec get_param_state (plst: param list): constr list * context list = 
    match plst with 
    | [] -> failwith "no params"
    | {name = s; p_type = None }:: [] -> 
      let ti = fresh_var () in 
      let clst = [] in 
      let context = [(s, ti)] in 
      clst, context
    | {name = s; p_type = Some t}:: [] -> 
      let ti = fresh_var () in 
      let clst = [(ti, t)] in 
      let context = [(s, ti)] in 
      clst, context
    | {name = s; p_type = None }:: tail -> 
      let clst, context = get_param_state tail in
      let ti = fresh_var () in 
      let context = (s, ti) :: context in 
      clst, context
    | {name = s; p_type = Some t}:: tail -> 
      let clst, context = get_param_state tail in
      let ti = fresh_var () in 
      let clst = (ti, t)::clst in 
      let context = (s, ti)::context in 
      clst, context

  (* Helper: Get function type from param context *)
  let rec get_function_type (context: context list) (ret_t: typ): typ = 
    match context with 
    | [] -> failwith "no function params"
    | (_, ti)::[]->  FuncTy(ti, ret_t)
    | (_, t1) :: tail -> FuncTy(t1, get_function_type tail ret_t)
  let rec typeof (e:expr) :typ = 
    let empty_state = {clst = []; context = []} in
    let st = typecheck e in 
    (* let st = unify st in *)
    eval_state st empty_state

  and typecheck: expr -> typ ConstrState.m = function

    | Binop(e1, op, e2) -> typecheck_binop e1 op e2
    | Unop(op, e) -> typecheck_unop op e
    | Tuple elst -> typecheck_tuple elst
    | IfExp(e1, e2, e3) -> typecheck_ifexp e1 e2 e3
    | LetExp(x, b, plst, t, e1, e2) -> typecheck_letexp x b plst t e1 e2
    | MatchExp(e, mlst) -> typecheck_matchexp e mlst
    | App(e1, e2) -> typecheck_app e1 e2
    | Function(plst, t, e) -> typecheck_function plst t e
    | CInt _ -> return IntTy
    | CString _ -> return StringTy
    | CBool _ -> return BoolTy
    | Unit -> return UnitTy
    (* For variable, look in context first, if not there / is free then gen new type var and add to context *)
    | Var s -> get >>= fun st ->
      let ty = List.assoc_opt s st.context in 
      let s_type = 
      (match ty with | Some t -> t | None -> fresh_var ()) in 
      let new_context = 
      (match ty with | Some t -> st.context | None -> (s, s_type) :: st.context) in 
      put {clst = st.clst; context = new_context } >>= fun _ ->
      return s_type
    | _ -> return UnitTy

    and typecheck_binop (e1: expr) (op: binop) (e2: expr): typ ConstrState.m  = 
      match op with 
      | LessThan -> 
        get >>= fun initial_state ->
        let t1, st1 = run_state (typecheck e1) initial_state in
        let t2, st2 = run_state (typecheck e2) initial_state in
        put {clst = [(t1, IntTy);(t2, IntTy)] @ st1.clst @ st2.clst ;
            context = merge_context [st1.context; st2.context]} >>= fun _ ->
        return BoolTy
      | Equal -> 
        get >>= fun initial_state ->
        let t1, st1 = run_state (typecheck e1) initial_state in
        let t2, st2 = run_state (typecheck e2) initial_state in
        put {clst = [(t1, t2)] @ st1.clst @ st2.clst ;
        context = merge_context [st1.context; st2.context]} >>= fun _ ->
        return BoolTy
      | StrConcat -> 
        get >>= fun initial_state ->
        let t1, st1 = run_state (typecheck e1) initial_state in
        let t2, st2 = run_state (typecheck e2) initial_state in
        put {clst = [(t1, StringTy);(t2, StringTy)] @ st1.clst @ st2.clst ;
            context = merge_context [st1.context; st2.context]} >>= fun _ ->
        return StringTy
      | LogicAnd | LogicOr -> 
        get >>= fun initial_state ->
        let t1, st1 = run_state (typecheck e1) initial_state in
        let t2, st2 = run_state (typecheck e2) initial_state in
        put {clst = [(t1, BoolTy);(t2, BoolTy)] @ st1.clst @ st2.clst ;
            context = merge_context [st1.context; st2.context]} >>= fun _ ->
        return BoolTy
      | _ -> 
        get >>= fun initial_state ->
        let t1, st1 = run_state (typecheck e1) initial_state in
        let t2, st2 = run_state (typecheck e2) initial_state in
        put {clst = [(t1, IntTy);(t2, IntTy)] @ st1.clst @ st2.clst ;
            context = merge_context [st1.context; st2.context]} >>= fun _ ->
        return IntTy

    and typecheck_unop (op: unop) (e: expr): typ ConstrState.m  = 
      match op with 
      | LogicNegate -> 
        get >>= fun initial_state ->
        let t, st = run_state (typecheck e) initial_state in
        put {clst = (t,BoolTy) :: st.clst ;
            context = st.context} >>= fun _ ->
        return BoolTy
      | IntNegate -> 
        get >>= fun initial_state ->
        let t, st = run_state (typecheck e) initial_state in
        put {clst = (t,IntTy) :: st.clst ;
            context = st.context} >>= fun _ ->
        return IntTy
    and typecheck_tuple (elst: expr list) : typ ConstrState.m =
      get >>= fun initial_state ->
      (* Helper: return list of types, and new constraint *)
      let rec get_typs_constr (elst: expr list): typ list * constr list = 
        (match elst with 
        | e1 :: e2 :: [] -> 
          let t1, st1 = run_state (typecheck e1) initial_state in
          let t2, st2 = run_state (typecheck e2) initial_state in
          [t1; t2] , merge_constr [st1.clst; st2.clst]
        | e1 :: tail -> 
          let t1, st1 = run_state (typecheck e1) initial_state in
          let typs, new_constr = get_typs_constr tail in
          t1 :: typs, merge_constr [st1.clst; new_constr]
        | _ -> failwith "not enough tuple elements"
        ) in
      let tlst, clst = get_typs_constr elst in 
      put {clst = merge_constr [clst; initial_state.clst];
          context = initial_state.context} >>= fun _ ->
        return (TupleTy tlst)
    and typecheck_ifexp (e1: expr) (e2: expr) (e3: expr): typ ConstrState.m  = 
      get >>= fun initial_state ->
      let t1, st1 = run_state (typecheck e1) initial_state in
      let t2, st2 = run_state (typecheck e2) initial_state in
      let t3, st3 = run_state (typecheck e3) initial_state in
      put {clst = [(t1,BoolTy); (t2,t3)] @ st1.clst @ st2.clst @ st3.clst ;
            context = merge_context [st1.context; st2.context; st3.context]} >>= fun _ ->
        return t2

    and typecheck_app (e1: expr) (e2: expr): typ ConstrState.m  = 
      get >>= fun initial_state ->
      let t1, st1 = run_state (typecheck e1) initial_state in
      let t2, st2 = run_state (typecheck e2) initial_state in
      let t3 = fresh_var () in 
      put {clst = [(t1,FuncTy (t2, t3))] @ st1.clst @ st2.clst;
            context = merge_context [st1.context; st2.context]} >>= fun _ ->
        return t3

    (* Add params to context, then typecheck e, return ret_t with original context *)
    and typecheck_function (plst: param list) (t: typ option) (e: expr) : typ ConstrState.m  = 
      get >>= fun initial_state ->
      let clst, context = get_param_state plst in 
      let function_state = {clst = initial_state.clst @ clst; context = merge_context [initial_state.context; context]} in
      let ret_t, st = run_state (typecheck e) function_state in
      let func_t = get_function_type context ret_t in
      put {clst = st.clst; context = initial_state.context} >>= fun _ ->
        return func_t

    and typecheck_matchexp (e: expr) (mlst: matchbranch list) = 
      failwith "undefined"

    and typecheck_letexp (x: string) (b:bool) (plst: param list) (t:typ option) (e1: expr) (e2:expr): typ ConstrState.m =
      failwith "undefined"

    and generalize (x: string) (st: typ ConstrState.m) : typ ConstrState.m = 
      failwith "undefined"
end

let typecheck (e: program) : typ = 
  failwith "undefined"

let typecheck_expr (e: expr) : typ = 
  TypeChecker.typeof e
