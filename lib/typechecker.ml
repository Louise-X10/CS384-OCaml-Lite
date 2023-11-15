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
  type 'a m = state -> 'a * state  (* current state -> result value * result state *)
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
  type constr = typ * typ
  type constr_state = {clst: constr list; context: (string*typ) list}
  module ConstrState = State(struct type state = constr_state end)
  open ConstrState

  let initial_state = {clst = []; context = []}
  let next_var = ref 0
  let fresh_var (_ : unit) : typ =
    let () = next_var := !next_var + 1 in
    CustomTy ("t$" ^ string_of_int !next_var)
  let rec typecheck: expr -> typ ConstrState.m = function

    | Binop(e1, op, e2) -> typecheck_binop e1 op e2
    | Unop(op, e) -> typecheck_unop op e
    | IfExp(e1, e2, e3) -> typecheck_ifexp e1 e2 e3
    | App(e1, e2) -> typecheck_app e1 e2
    | CInt _ -> return IntTy
    | CString _ -> return StringTy
    | CBool _ -> return BoolTy
    | Unit -> return UnitTy
    | Var s -> get >>= fun st ->
      let t = fresh_var () in 
      put {clst = st.clst; context = [(s, t)]} >>= fun _ ->
      return t  
    | _ -> return UnitTy

    and typecheck_binop (e1: expr) (op: binop) (e2: expr): typ ConstrState.m  = 
      match op with 
      | LessThan -> 
        let t1, st1 = run_state (typecheck e1) initial_state in
        let t2, st2 = run_state (typecheck e2) initial_state in
        put {clst = [(t1, IntTy);(t2, IntTy)] @ st1.clst @ st2.clst ;
            context = []} >>= fun _ ->
        return BoolTy
      | Equal -> 
        let t1, st1 = run_state (typecheck e1) initial_state in
        let t2, st2 = run_state (typecheck e2) initial_state in
        put {clst = [(t1, t2)] @ st1.clst @ st2.clst ;
        context = []} >>= fun _ ->
        return BoolTy
      | StrConcat -> 
        let t1, st1 = run_state (typecheck e1) initial_state in
        let t2, st2 = run_state (typecheck e2) initial_state in
        put {clst = [(t1, StringTy);(t2, StringTy)] @ st1.clst @ st2.clst ;
            context = []} >>= fun _ ->
        return StringTy
      | LogicAnd | LogicOr -> 
        let t1, st1 = run_state (typecheck e1) initial_state in
        let t2, st2 = run_state (typecheck e2) initial_state in
        put {clst = [(t1, BoolTy);(t2, BoolTy)] @ st1.clst @ st2.clst ;
            context = []} >>= fun _ ->
        return BoolTy
      | _ -> 
        let t1, st1 = run_state (typecheck e1) initial_state in
        let t2, st2 = run_state (typecheck e2) initial_state in
        put {clst = [(t1, IntTy);(t2, IntTy)] @ st1.clst @ st2.clst ;
            context = []} >>= fun _ ->
        return IntTy

    and typecheck_unop (op: unop) (e: expr): typ ConstrState.m  = 
      match op with 
      | LogicNegate -> 
        let t, st = run_state (typecheck e) initial_state in
        put {clst = (t,BoolTy) :: st.clst ;
            context = []} >>= fun _ ->
        return BoolTy
      | IntNegate -> 
        let t, st = run_state (typecheck e) initial_state in
        put {clst = (t,IntTy) :: st.clst ;
            context = []} >>= fun _ ->
        return IntTy
    and typecheck_ifexp (e1: expr) (e2: expr) (e3: expr): typ ConstrState.m  = 
      let t1, st1 = run_state (typecheck e1) initial_state in
      let t2, st2 = run_state (typecheck e2) initial_state in
      let t3, st3 = run_state (typecheck e3) initial_state in
      put {clst = [(t1,BoolTy); (t2,t3)] @ st1.clst @ st2.clst @ st3.clst ;
            context = []} >>= fun _ ->
        return t2

    and typecheck_app (e1: expr) (e2: expr): typ ConstrState.m  = 
      let t1, st1 = run_state (typecheck e1) initial_state in
      let t2, st2 = run_state (typecheck e2) initial_state in
      let t3 = fresh_var () in 
      put {clst = [(t1,FuncTy (t2, t3))] @ st1.clst @ st2.clst;
            context = []} >>= fun _ ->
        return t3
    
end

let typecheck (e: program) : typ = 
    failwith "undefined"

let typecheck_expr (e: expr) : typ = 
  failwith "undefined"
