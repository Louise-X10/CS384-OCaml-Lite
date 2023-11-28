open OUnit2
open Ocaml_lite.Lexer
open Ocaml_lite.Ast 
open Ocaml_lite.Interpret
(* 

open Interpretor*)


let interp_expr_tests = "test suite for interpretor on expressions" >::: [
   
  "arithmetic computations" >::
  (fun _ -> assert_equal
      (VInt 2)
      (interp_expr (parse_expr "(1 / 2 + 3 * 4 - 4 ) mod 6")));
    
  "comparisons" >::
  (fun _ -> assert_equal
      (VBool true)
      (interp_expr (parse_expr "(2 + 3 < 7) = true")));

  "logic expressions" >::
  (fun _ -> assert_equal
      (VBool true)
      (interp_expr (parse_expr "not true && false || true")));

  "string concat" >::
  (fun _ -> assert_equal
      (VString "helloworld")
      (interp_expr (parse_expr " \"hello\" ^ \"world\" ")));

  "integer negate" >::
  (fun _ -> assert_equal
      (VInt 1)
      (interp_expr (parse_expr  "~1 + 2")));

    "if expression (simple)" >::
    (fun _ -> assert_equal
        (VBool false)
        (interp_expr (parse_expr "if 3 < 2 then true else false")));

    "tuple type" >::
    (fun _ -> assert_equal
        (VTuple [VInt 1; VInt 2; VInt 3])
        (interp_expr (parse_expr "(1, 2, 3)")));

    "match expr with given context: with pattern vars" >::
    (fun _ -> assert_equal 
        (VInt 3)
        (let e = parse_expr  "match p with | Pair (x, y) => x + y" in
        let state = Interpretor.interp_expr e in
        Interpretor.InterpState.eval_state state  {env = [("p", VUser("Pair", [VInt 1; VInt 2]))]}
        ));

    "match expr with given context: no pattern vars" >::
    (fun _ -> assert_equal 
        (VInt 0)
        (let e = parse_expr  "match p with | Pair (x, y) => x + y | None => 0" in
        let state = Interpretor.interp_expr e in
        Interpretor.InterpState.eval_state state  {env = [("p", VUser("None", []))]}
        ));

    "function expression: single arg" >::
    (fun _ -> assert_equal
        (VClosure ("x", [], Binop(Var "x",Add, CInt 1), None))
        (interp_expr (parse_expr "fun x => x + 1")));

    "function expression: multiple arg" >::
    (fun _ -> assert_equal
        (VClosure ("x", [], 
        Function([{ name="y"; p_type=None}], None, Binop(Var "x", Add, Var "y")),
        None))
        (interp_expr (parse_expr "fun x y => x + y")));

    "function application with given context: return another function/closure" >::
    (fun _ -> assert_equal 
        (VClosure ("y", [("x", VInt 1)], Binop(Var "x", Add, Var "y"), None))
        (let f = interp_expr (parse_expr "fun x y => x + y") in 
        let e = parse_expr  "f 1" in
        let state = Interpretor.interp_expr e in
        Interpretor.InterpState.eval_state state  {env = [("f", f)]}
        ));

    "function application with given context: return final value" >::
    (fun _ -> assert_equal 
        (VInt 4)
        (let f = interp_expr (parse_expr "fun x y => x + y") in 
        let e = parse_expr  "(f 1) 3" in
        let state = Interpretor.interp_expr e in
        Interpretor.InterpState.eval_state state  {env = [("f", f)]}
        ));

    "let expression: no type" >::
    (fun _ -> assert_equal
        (VInt 1)
        (interp_expr (parse_expr  "let x = 0 in x + 1")));

    "ill-typed" >::
    (fun _ -> try
        let _ = interp_expr (parse_expr "1 || true") in
        assert_failure "'1 || true' passed the interpretor"
    with
    | InterpError _ -> assert_bool "" true
    | _ -> assert_failure "Unexpected error");

    "function application from let expression: with type" >::
    (fun _ -> assert_equal
        (VBool false)
        (interp_expr (parse_expr 
            "let f (x : int) : bool = if x < 0 then true else false in f 1")));
  ]
  let interp_binding_tests_hl = "test suite for interpretor on bindings (high-level)" >::: [

    "construct user-defined type: multiple pvar" >::
    (fun _ -> assert_equal
        ( VUser("Pair", [VInt 1; VInt 2]))
        (let st = interp_program (parse "type pairing = | Pair of int * int | Single of int | Null;; 
                                        let a = Pair (1, 2);;") in 
        List.assoc "a" st.env));
    
    "construct user-defined type: single pvar" >::
    (fun _ -> assert_equal
        ( VUser("Single", [VInt 1]))
        (let st = interp_program (parse "type pairing = | Pair of int * int | Single of int | Null;; 
                                        let a = Single 1;;") in 
        List.assoc "a" st.env));

    "construct user-defined type: single pvar" >::
    (fun _ -> assert_equal
        ( VUser("Null", []))
        (let st = interp_program (parse "type pairing = | Pair of int * int | Single of int | Null;; 
                                        let a = Null;;") in 
        List.assoc "a" st.env));
      
    "function application with let binding: return another function/closure" >::
    (fun _ -> assert_equal
        (VClosure ("y", [("x", VInt 1)], Binop(Var "x", Add, Var "y"), None))
        (let st = interp_program (parse 
        "let f = fun x => fun y => x + y;;
        let g = f 1;;") in 
        List.assoc "g" st.env
        ));

    "function application with let binding: return final value" >::
    (fun _ -> assert_equal
        (VInt 4)
        (let st = interp_program (parse 
        "let f = fun x => fun y => x + y;;
        let g = (f 1) 3;;") in 
        List.assoc "g" st.env
        ));
        
    "function application from let binding, with type" >::
    (fun _ -> assert_equal
        (VBool false)
        (let st = interp_program (parse 
            "let f (x : int) : bool = if x < 0 then true else false;;
                let result = f 1;;") in 
                List.assoc "result" st.env
        ));

    "match expression" >::
    (fun _ -> assert_equal
        (VInt 3)
        (let st = interp_program (parse "
        type pairing = | Pair of int * int;;
        let p : pairing = Pair (1, 2);;
        let result = match p with | Pair (x, y) => x + y;;") in 
        List.assoc "result" st.env
        ));

    (* "nested match expression" >::
    (fun _ -> assert_equal
        (VInt 1)
        (let st = interp_program (parse  "
        type pairing = | Pair of int * int | Single of int;;
        let p : pairing = Pair (Single 1, Single 2);;
        let result = match p with Pair (x, y) => match x with | Single i => i ;;") in 
        List.assoc "result" st.env
        )); *)
  ]
  let interp_binding_tests_ll = "test suite for interpretor on bindings (low-level)" >::: [

    "define type binding: no pvar" >::
    (fun _ -> assert_equal
        (VUnit, [
            ("Null", VUser("Null", []))
        ])
        (let st = Interpretor.interp_binding (List.hd (
            (parse "type pairing = | Null;;"))) in 
            let ret_v, ret_st = Interpretor.InterpState.run_state st Interpretor.empty_st in 
            ret_v, ret_st.env 
        ));

    "construct user-defined type: no pvar" >::
    (fun _ -> assert_equal
        (VUser("Null", []))
        (let b1 = List.hd ((parse "type pairing = | Null;;")) in 
        let b2 = (parse_expr "Null") in 
        let st1 = Interpretor.interp_binding b1 in 
        let st2 = Interpretor.interp_expr b2 in 
        let _, ret_state = Interpretor.InterpState.run_state st1 Interpretor.empty_st in 
        let ret_v2, _ = Interpretor.InterpState.run_state st2 ret_state in 
            ret_v2
        ));

    "construct user-defined type: single pvar" >::
    (fun _ -> assert_equal
        (VUser("Single", [VInt 1]))
        (let b1 = List.hd ((parse "type pairing = | Single of int;;")) in 
        let b2 = (parse_expr "Single 1") in 
        let st1 = Interpretor.interp_binding b1 in 
        let st2 = Interpretor.interp_expr b2 in 
        let ret_state = Interpretor.InterpState.exec_state st1 Interpretor.empty_st in 
        let ret_v2 = Interpretor.InterpState.eval_state st2 ret_state in 
            ret_v2
        ));

    "let binding: no params" >::
    (fun _ -> assert_equal
        (VUnit, [
            ("x", VInt 0)
        ])
        (let st = Interpretor.interp_binding (List.hd (
            (parse "let x = 0;;"))) in 
            let ret_v, ret_st = Interpretor.InterpState.run_state st Interpretor.empty_st in 
            ret_v, ret_st.env));

    "let binding: with params" >::
    (fun _ -> assert_equal
        (VUnit, [
            ("f", VClosure ("x", [], IfExp(Binop(Var "x", LessThan, CInt 0), CBool true, CBool false), None))
        ])
        (let st = Interpretor.interp_binding (List.hd (
            (parse "let f (x : int) : bool = if x < 0 then true else false;;"))) in 
            let ret_v, ret_st = Interpretor.InterpState.run_state st Interpretor.empty_st in 
            ret_v, ret_st.env));
  ]

  let interp_tests = "test_suite for interpretor" >::: [
  interp_expr_tests;
  interp_binding_tests_ll;
  interp_binding_tests_hl;
]
