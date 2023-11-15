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
  module ConstrState = State(struct type state = constr list end)
  open ConstrState

  let initial_state = []
  let rec typecheck: expr -> typ ConstrState.m = function

    | Binop(e1, op, e2) -> typecheck_binop e1 op e2
    | Unop(op, e) -> typecheck_unop op e
    | CInt _ -> return IntTy
    | CString _ -> return StringTy
    | CBool _ -> return BoolTy
    | Unit -> return UnitTy
    | _ -> return UnitTy

    and typecheck_binop (e1: expr) (op: binop) (e2: expr): typ ConstrState.m  = 
      match op with 
      | LessThan -> 
        let t1, clst1 = run_state (typecheck e1) initial_state in
        let t2, clst2 = run_state (typecheck e2) initial_state in
        put ([(t1, IntTy);(t2, IntTy)] @ clst1 @ clst2) >>= fun _ ->
        return BoolTy
      | Equal -> 
        let t1, clst1 = run_state (typecheck e1) initial_state in
        let t2, clst2 = run_state (typecheck e2) initial_state in
        put ([(t1, t2)] @ clst1 @ clst2) >>= fun _ ->
        return BoolTy
      | StrConcat -> 
        let t1, clst1 = run_state (typecheck e1) initial_state in
        let t2, clst2 = run_state (typecheck e2) initial_state in
        put ([(t1, StringTy);(t2, StringTy)] @ clst1 @ clst2) >>= fun _ ->
        return StringTy
      | LogicAnd | LogicOr -> 
        let t1, clst1 = run_state (typecheck e1) initial_state in
        let t2, clst2 = run_state (typecheck e2) initial_state in
        put ([(t1, BoolTy);(t2, BoolTy)] @ clst1 @ clst2) >>= fun _ ->
        return BoolTy
      | _ -> 
        let t1, clst1 = run_state (typecheck e1) initial_state in
        let t2, clst2 = run_state (typecheck e2) initial_state in
        put ([(t1, IntTy);(t2, IntTy)] @ clst1 @ clst2) >>= fun _ ->
        return IntTy

    and typecheck_unop (op: unop) (e: expr): typ ConstrState.m  = 
      match op with 
      | LogicNegate -> 
        let t, clst = run_state (typecheck e) initial_state in
        put ((t,BoolTy) :: clst) >>= fun _ ->
        return BoolTy
      | IntNegate -> 
        let t, clst = run_state (typecheck e) initial_state in
        put ((t,IntTy) :: clst) >>= fun _ ->
        return IntTy

end

let typecheck (e: program) : typ = 
    failwith "undefined"

let typecheck_expr (e: expr) : typ = 
  failwith "undefined"
