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
(* Use _st to name constr_state variables. *)

module State (S : sig type state end) : sig
  include MONAD
  val get : S.state m
  val put : S.state -> unit m
  val run_state : 'a m -> S.state -> 'a * S.state
  val eval_state : 'a m -> S.state -> 'a
  val exec_state : 'a m -> S.state -> S.state
  val chain_states: 'a m list -> S.state -> 'a list * S.state
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
  
  (* Apply list of states in order, then return list of return values + current state *)
  let rec chain_states (st_lst: 'a m list) (init_st: state): 'a list * state = 
    match st_lst with 
    | [] -> failwith "Error in chaining binding states"
    | [st] -> 
      let b_typ, b_st = run_state st init_st in 
      [b_typ], b_st
    | st :: tail -> 
      let b_typ, b_st = run_state st init_st in 
      let b_typs, ret_st = chain_states tail b_st in 
      b_typ :: b_typs, ret_st
end

module TypeChecker = struct
  type constr = typ * typ (* e.g. (t0, int) *)
  type context = string * typ (* e.g. (x, t0) means x bound to t0 *)
  type constr_state = {clst: constr list; context: context list}
  let empty_st = {clst = []; context = []}
  module ConstrState = State(struct type state = constr_state end)
  type sub = typ * typ (* e.g. (t0, int) means map t0 to int *)
  open ConstrState

  (* Generate new type variables *)
  let next_var = ref 0
  let fresh_var (_ : unit) : typ =
    let () = next_var := !next_var + 1 in
    VarTy (!next_var)

  (* Helper: get list of free type variables in given t *)
  let rec get_varty (t: typ): int list = 
    match t with 
    | VarTy i -> [i]
    | FuncTy(arg, body) -> (get_varty arg) @ (get_varty body)
    | TupleTy tlst -> List.fold_left (@) [] (List.map (fun t -> get_varty t) tlst)
    | _ -> []

  (* Helper: only keep type variables that are not free in context *)
  let remove_varty (varty_lst: int list) (context_lst: context list): int list = 
    let _, typ_lst = List.split context_lst in 
    (* If varty is not in typ_lst, then return true *)
    let vartyFree (varty): bool = not (List.mem (VarTy varty) typ_lst) in 
      List.find_all vartyFree varty_lst

  (* Check if t1 occurs in t2 *)
  let rec occurs_check (t1: typ) (t2: typ): bool = 
    match t2 with 
    | FuncTy(arg, body) -> 
      if t1 = arg then true else occurs_check t1 body
    | TupleTy tlst -> List.exists (fun t -> occurs_check t1 t) tlst
    | VarTy _ as t -> if t1 = t then true else false
    | _ -> false

  (* Helper: apply sub (i.e. map any occurance of t1 to t2) in target_t*)
  let rec sub_type (subst: sub) (target_t: typ): typ = 
    match target_t with 
    | FuncTy(arg, body) -> 
      let new_arg = (if arg = fst subst then snd subst else arg) in 
      let new_body = sub_type subst body in
      FuncTy(new_arg, new_body)
    | TupleTy tlst -> TupleTy (List.map (fun t -> sub_type subst t) tlst)
    | VarTy _ as t -> if t = fst subst then snd subst else t
    | t -> t

  (* Helper: apply single sub to single constraint *)
  let sub_type_in_constr (subst: sub) (c: constr): constr = 
    let t1 = fst c in 
    let t2 = snd c in 
    sub_type subst t1, sub_type subst t2

  (* Helper: apply single sub to constraint list *)
  let sub_type_all_constrts (clst: constr list) (subst:sub): constr list = 
    List.map (fun c -> sub_type_in_constr subst c) clst

  (* Helper: unify single constraint, return list of new constraints or new substitutions *)
  let unify_single_constr: constr -> constr list * sub list = function
    | VarTy t1, VarTy t2 -> 
      if t1 <> t2 then [], [(VarTy t1, VarTy t2)]
      else  [], []
    | (VarTy _ as t1), t2 | t2, (VarTy _ as t1) -> 
      if occurs_check t1 t2 = true then raise (TypeError ("Infinite unification")) else
      [], [(t1, t2)]
    | FuncTy (arg1, body1), FuncTy (arg2, body2) -> 
      [(arg1, arg2); (body1, body2)], []
    | TupleTy tls1, TupleTy tls2 -> 
      List.combine tls1 tls2, []
    | ForallTy (i, t1), t2 -> 
      let new_t = fresh_var () in 
      let new_constraint = (sub_type (VarTy i, new_t) t1), t2 in 
      [new_constraint], []
    | t1, t2 -> if t1 <> t2 then raise (TypeError ("Unification failed")) else [], []
  
  (* Helper: unify all constraints to build sub list *)
  let rec unify_multiple_constr (clst: constr list) (sub: sub list): constr list * sub list = 
    match clst with 
    (* stop if clst is empty *)
    | [] -> [], sub
    (* apply first constraint, recurse on remaining clst *)
    | (t1, t2) :: tl_clst -> 
    let new_constr, new_sub = unify_single_constr (t1, t2) in
    let sub = new_sub @ sub in
    let clst = new_constr @ tl_clst in 
    (* apply new_sub to all constraints *) 
    let clst = if new_sub <> [] then (List.fold_left sub_type_all_constrts clst new_sub) else clst in 
    unify_multiple_constr clst sub
    
  (* Helper: apply list of substitutions to t once *)
  let rec apply_subst_oneround (slst: sub list) (t: typ): typ = 
    match slst with 
    | [] -> t
    | sub :: [] -> sub_type sub t
    | sub :: tail -> 
      let ret_t = sub_type sub t in 
      apply_subst_oneround tail ret_t

  (* Helper: apply list of substitutions to t until it doesn't change *)
  let rec apply_subst (slst: sub list) (t: typ): typ = 
    let init_t = t in 
    let res_t = apply_subst_oneround slst t in 
    if init_t = res_t then res_t else apply_subst slst res_t

  (* Unify constraint list to give actual return type *)
  let unify (clst: constr list) (t: typ): typ = 
    let res_clst, res_slst = unify_multiple_constr clst [] in 
    (* Assert that res_clst must be empty *)
    let _ = if res_clst = [] then () else (raise (TypeError ("Unresolved constraints remain!"))) in 
    let ret_t = apply_subst res_slst t in 
    ret_t

  (* Helper: Merge multiple context lists without duplicates *)
  let merge_context (lst_of_context: context list list)  = 
    (* let comp = (fun (x, _) (y, _) -> String.compare x y) in *)
    let contexts = List.fold_left (@) [] lst_of_context in 
    List.sort_uniq compare contexts

  (* Helper: Merge multiple constraint lists without duplicates *)
  let merge_clst (lst_of_constr: constr list list)  = 
    (* let comp = (fun (x, x') (y, y') -> String.compare x y) in *)
    let constraints = List.fold_left (@) [] lst_of_constr in 
    List.sort_uniq compare constraints

  (* Helper: Get function's initial state from param list. Must have at least 1 param. 
    See (x: int), return {clst = [(t0, int)]; context = [(x, t0)]}
    See (x), return {clst = []; context = [(x, t0)]}
  *)
  let rec get_param_state (plst: param list): constr list * context list = 
    match plst with 
    | [] -> raise (TypeError ("Need at least one function parameters"))
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

  (* Helper (for matchexp): Get context from constructor and pattern vars  *)
  let get_pvar_context (plst_opt: pattern_vars option) (constructor_type: typ): context list = 
    (* Assign each pvar in plst to a type in pvars_tlst *)
    let rec helper (plst: pattern_vars) (pvars_tlst: typ list): context list = 
      (match pvars_tlst with 
      | [] -> if plst <> [] then raise (TypeError ("Not enough pattern variables for type constructor")) else []
      | ti::[]->  [(List.hd plst, ti)]
      | t1 :: tail -> (List.hd plst, t1) :: helper (List.tl plst) tail
      ) in 
    match plst_opt with 
    (* If no pvar, context is empty *)
    | None -> []
    (* If has pvar, context is each pvar assigned to typ of arg in constructor definition *)
    | Some plst -> 
      (match constructor_type with 
      | FuncTy(TupleTy pvars_tlst, _) -> helper plst pvars_tlst
      | FuncTy(pvar_t, _) -> helper plst [pvar_t]
      | _ -> raise (TypeError "Type Constructor is ill-typed"))

  (* Helper: Get pattern type from constructor type *)
  let get_ptype: typ -> typ = function
      (* If has pvar, pattern type = the final return type of constructor type *)
      | FuncTy(_, ret_t) -> ret_t
      | TupleTy _ | VarTy _ | ForallTy (_ , _) -> raise (TypeError "Type Constructor is ill-typed, not function")
      (* If no pvar, pattern type = constructor type *)
      | _ as t -> t 
    
  (* Helper: Get list of constaints that each type of subexpr must match *)
  let get_subexpr_clst (tlst: typ list): constr list = 
    let lst1 = List.rev (List.tl (List.rev tlst)) in 
    let lst2 = List.tl tlst in 
    List.map2 (fun t1 t2 -> (t1, t2)) lst1 lst2

  (* Helper: Get function type from param context *)
  let rec get_function_type (context: context list) (ret_t: typ): typ = 
    match context with 
    | [] -> raise (TypeError ("Need at least one function parameter"))
    | (_, ti)::[]->  FuncTy(ti, ret_t)
    | (_, t1) :: tail -> FuncTy(t1, get_function_type tail ret_t)
  
  (* Helper: Get constraint if user provides return type annotation *)
  let get_ret_constraint (t: typ option) (ret_t: typ): constr list = 
    match t with 
    | None -> []
    | Some t -> [(ret_t, t)]

  let rec typeof_program (blst: program): typ list = 
    let st_lst = List.map typecheck_binding blst in 
    let ret_typs, _ = chain_states st_lst empty_st in 
    ret_typs

  and typeof_binding (b: binding) :typ = 
    let st = typecheck_binding b in 
    let ret_t, ret_st = run_state st empty_st in 
    let ret_t = unify ret_st.clst ret_t in 
    ret_t

  and typeof_expr (e:expr) :typ = 
    let st = typecheck_expr e in 
    let ret_t, ret_st = run_state st empty_st in 
    let ret_t = unify ret_st.clst ret_t in 
    ret_t

  and typecheck_binding: binding -> typ ConstrState.m = function
    | LetB(x, b, plst, t, e1) -> typecheck_letbinding x b plst t e1
    | TypeB(x, tblst) -> typecheck_typebinding x tblst

  and typecheck_expr: expr -> typ ConstrState.m = function

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
    | Var s -> 
      (
        match s with 
        | "int_of_string" -> return (FuncTy (IntTy, StringTy))
        | "string_of_int" -> return (FuncTy (StringTy, IntTy))
        | "print_string" -> return (FuncTy (StringTy, UnitTy))
        | _ -> get >>= fun st ->
          let ty = List.assoc_opt s st.context in 
          let s_type = 
          (match ty with | Some t -> t | None -> fresh_var ()) in 
          let new_context = 
          (match ty with | Some _ -> st.context | None -> (s, s_type) :: st.context) in 
          put {clst = st.clst; context = new_context } >>= fun _ ->
          return s_type
      )
      

    and typecheck_binop (e1: expr) (op: binop) (e2: expr): typ ConstrState.m  = 
      match op with 
      | LessThan -> 
        get >>= fun initial_st ->
        let t1, st1 = run_state (typecheck_expr e1) initial_st in
        let t2, st2 = run_state (typecheck_expr e2) initial_st in
        put {clst = merge_clst [[(t1, IntTy);(t2, IntTy)]; st1.clst; st2.clst] ;
            context = initial_st.context} >>= fun _ ->
        return BoolTy
      | Equal -> 
        get >>= fun initial_st ->
        let t1, st1 = run_state (typecheck_expr e1) initial_st in
        let t2, st2 = run_state (typecheck_expr e2) initial_st in
        put {clst = merge_clst [[(t1, t2)]; st1.clst; st2.clst] ;
        context = initial_st.context} >>= fun _ ->
        return BoolTy
      | StrConcat -> 
        get >>= fun initial_st ->
        let t1, st1 = run_state (typecheck_expr e1) initial_st in
        let t2, st2 = run_state (typecheck_expr e2) initial_st in
        put {clst = merge_clst [[(t1, StringTy);(t2, StringTy)]; st1.clst; st2.clst] ;
            context = initial_st.context} >>= fun _ ->
        return StringTy
      | LogicAnd | LogicOr -> 
        get >>= fun initial_st ->
        let t1, st1 = run_state (typecheck_expr e1) initial_st in
        let t2, st2 = run_state (typecheck_expr e2) initial_st in
        put {clst = merge_clst [[(t1, BoolTy);(t2, BoolTy)]; st1.clst; st2.clst] ;
            context = initial_st.context} >>= fun _ ->
        return BoolTy
      | _ -> 
        get >>= fun initial_st ->
        let t1, st1 = run_state (typecheck_expr e1) initial_st in
        let t2, st2 = run_state (typecheck_expr e2) initial_st in
        put {clst = merge_clst [[(t1, IntTy);(t2, IntTy)]; st1.clst; st2.clst] ;
            context = initial_st.context} >>= fun _ ->
        return IntTy

    and typecheck_unop (op: unop) (e: expr): typ ConstrState.m  = 
      match op with 
      | LogicNegate -> 
        get >>= fun initial_st ->
        let t, st = run_state (typecheck_expr e) initial_st in
        put {clst = (t,BoolTy) :: st.clst ;
            context = initial_st.context} >>= fun _ ->
        return BoolTy
      | IntNegate -> 
        get >>= fun initial_st ->
        let t, st = run_state (typecheck_expr e) initial_st in
        put {clst = (t,IntTy) :: st.clst ;
            context = initial_st.context} >>= fun _ ->
        return IntTy
    and typecheck_tuple (elst: expr list) : typ ConstrState.m =
      get >>= fun initial_st ->
      (* Helper: return list of types, and new constraint *)
      let rec get_typs_constr (elst: expr list): typ list * constr list = 
        (match elst with 
        | e1 :: e2 :: [] -> 
          let t1, st1 = run_state (typecheck_expr e1) initial_st in
          let t2, st2 = run_state (typecheck_expr e2) initial_st in
          [t1; t2] , merge_clst [st1.clst; st2.clst]
        | e1 :: tail -> 
          let t1, st1 = run_state (typecheck_expr e1) initial_st in
          let typs, new_constr = get_typs_constr tail in
          t1 :: typs, merge_clst [st1.clst; new_constr]
        | _ -> raise (TypeError ("Need more than one tuple elements"))
        ) in
      let tlst, clst = get_typs_constr elst in 
      put {clst = merge_clst [clst; initial_st.clst];
          context = initial_st.context} >>= fun _ ->
        return (TupleTy tlst)
    and typecheck_ifexp (e1: expr) (e2: expr) (e3: expr): typ ConstrState.m  = 
      get >>= fun initial_st ->
      let t1, st1 = run_state (typecheck_expr e1) initial_st in
      let t2, st2 = run_state (typecheck_expr e2) initial_st in
      let t3, st3 = run_state (typecheck_expr e3) initial_st in
      put {clst = [(t1,BoolTy); (t2,t3)] @ st1.clst @ st2.clst @ st3.clst ;
            context = initial_st.context} >>= fun _ ->
        return t2
    and typecheck_app (e1: expr) (e2: expr): typ ConstrState.m  = 
      get >>= fun initial_st ->
      let t1, st1 = run_state (typecheck_expr e1) initial_st in
      let t2, st2 = run_state (typecheck_expr e2) initial_st in
      let t3 = fresh_var () in 
      put {clst = [(t1,FuncTy (t2, t3))] @ st1.clst @ st2.clst;
            context = initial_st.context} >>= fun _ ->
        return t3

    (* Add params to context, then typecheck e, return ret_t with original context *)
    and typecheck_function (plst: param list) (t: typ option) (e: expr) : typ ConstrState.m  = 
      get >>= fun initial_st ->
      let param_clst, param_context = get_param_state plst in 
      let function_st = {clst = initial_st.clst; context = merge_context [initial_st.context; param_context]} in
      let ret_t, ret_st = run_state (typecheck_expr e) function_st in
      let ret_constraint = get_ret_constraint t ret_t in 
      let func_t = get_function_type param_context ret_t in
      put {clst = merge_clst [ret_constraint; ret_st.clst; param_clst]; 
          context = initial_st.context} >>= fun _ ->
        return func_t

    (* type check match expr e = e_typ, use updated context to evaluate branches??  *)
    (* For each branch, add constraint type of pattern constructor p_type ~ t.
      Add pattern vars to context and check subexpr, return type of subexpr *)
      (* e, p1, p2, ..., pn *)
    (* Add constraint that all subexpr types match *)
    and typecheck_matchexp (e: expr) (brlst: matchbranch list): typ ConstrState.m = 
      get >>= fun initial_st ->
      let e_typ, _ = run_state (typecheck_expr e) initial_st in 

      let typecheck_matchbr (br: matchbranch): typ ConstrState.m = 
        (match br with | MatchBr (s, pvars, e) -> 
          let constructor_type = List.assoc s initial_st.context in
          (* create pvar context: give each pvar a type according to constructor def *)
          let pvar_context = get_pvar_context pvars constructor_type in 
          (* evaluate subexpr in pvar context to get return type*)
          let branch_st = {clst = initial_st.clst; 
                              context = merge_context [pvar_context; initial_st.context]} in 
          let ret_t, ret_st = run_state (typecheck_expr e) branch_st in 
          (* Add constraint: type of pattern must match type of constructor, i.e. match type of e, p1, p2, ..., pn *)
          let p_typ = get_ptype constructor_type in 
          let new_constraint = (e_typ, p_typ) in 
          (* return ret_typ of subexpr, restore initial context, update constraints *)
          put {clst = merge_clst [ [new_constraint]; ret_st.clst];
              context = initial_st.context} >>= fun _ ->
          return  ret_t) in

      let st_lst = (List.map typecheck_matchbr brlst) in 
      (* Add constraints: Subexpr return type must match, i.e. match type of e1, e2, ..., en *)
      let subexpr_tslt = (List.map (fun st -> eval_state st initial_st) st_lst) in 
      let subexpr_clst = get_subexpr_clst subexpr_tslt in 

      (* Combine pattern constructor constraints from branches *)
      let list_of_clst = (List.map (fun st -> 
                            let res_st = exec_state st initial_st in res_st.clst) st_lst) in
      
      put {clst = merge_clst (subexpr_clst :: list_of_clst);
          context = initial_st.context} >>= fun _ -> 
      return (List.hd subexpr_tslt)
    
    (* Use context with {x ~ generalized type} added when checking e2, 
       return new constraint list with original context *)
    and typecheck_letexp (x: string) (b:bool) (plst: param list) (t:typ option) (e1: expr) (e2:expr): typ ConstrState.m =
      get >>= fun initial_st -> 
        let _, e2_st = run_state (typecheck_letbinding x b plst t e1) initial_st in 
        let ret_t, ret_st = run_state (typecheck_expr e2) e2_st in 
        put { clst = ret_st.clst; 
              context = initial_st.context } >>= fun _ ->
        return ret_t

    (* Evaluate type of e1, unify constraints, generalize, 
       add x ~ generalized type to context *)
    and typecheck_letbinding (x: string) (b:bool) (plst: param list) (t:typ option) (e1: expr): typ ConstrState.m =
      get >>= fun initial_st -> 
        (* If recursive, then give x a new type var when checking e1 *)
        let e1_initial_st = 
          (if b = true 
            then {clst = initial_st.clst; 
                  context = merge_context[[(x, fresh_var ())]; initial_st.context]} 
            else initial_st) in
        (* If there is plst, then check like a function. Otherwise, check like an expression *)
        let x_type, e1_st = 
          if List.length plst > 0 then run_state (typecheck_function plst t e1) e1_initial_st else 
            let e1_type, e1_temp_st = run_state (typecheck_expr e1) e1_initial_st in 
            (* Add optional user constraint on return type *)
            let e1_ret_st = {clst = merge_clst[(get_ret_constraint t e1_type); e1_temp_st.clst]; 
            context = e1_temp_st.context} in 
            e1_type, e1_ret_st in 
        (* Unify then generalize *)
        let x_type = unify e1_st.clst x_type in 
        let gen_x_type = generalize e1_st.context x_type in
        (* Add generalized type to context. No clst after unify *)
        put { clst = []; 
              context = merge_context [[(x, gen_x_type)]; initial_st.context] } >>= fun _ -> 
        return gen_x_type

    and generalize (context_lst: context list) (x_type: typ) : typ = 
      (* Get all varty that appear in the x_type constraint *)
      let varty_lst = get_varty x_type in 
      let varty_lst = List.sort_uniq compare varty_lst in
      (* Remove varty that appear in context *)
      let varty_lst = remove_varty varty_lst context_lst in 
      (* Apply Forall to each free var *)
      let helper (x_type: typ) (varty: int) : typ = ForallTy(varty, x_type) in 
      List.fold_left helper x_type varty_lst 

    (* type pairing = | Pair of int * int has type (int*int) -> pairing *)
    and typecheck_typebinding (s: string) (tblst: typ_binding list): typ ConstrState.m =
      get >>= fun initial_st -> 
        let def_t = UserTy s in 
        let context_lst = List.map (fun tb -> get_tb_type tb def_t) tblst in 
        put {clst = initial_st.clst; context = merge_context[initial_st.context; context_lst]} >>= fun _ ->
          return def_t

    and get_tb_type (tb: typ_binding) (def_t: typ): context = 
      let constructor = fst tb in 
      match snd tb with 
      | None -> (constructor, def_t)
      | Some constructor_vars -> 
        let tb_type = (
          match constructor_vars with 
          | TupleTy _ as t -> FuncTy(t, def_t)
          | FuncTy _ | VarTy _ | ForallTy _  -> 
            raise (TypeError ("Type Binding's Constructor variable types should be tuple of types or single type"))
          | _ as t -> FuncTy(t, def_t)
        ) in 
        (constructor, tb_type)

end

let typecheck (e: program) : typ list = 
  TypeChecker.typeof_program e
let typecheck_binding (e: binding) : typ = 
  TypeChecker.typeof_binding e
let typecheck_expr (e: expr) : typ = 
  TypeChecker.typeof_expr e
