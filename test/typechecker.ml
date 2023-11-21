open OUnit2
open Ocaml_lite.Lexer
open Ocaml_lite.Ast
open Ocaml_lite.Typechecker

let type_expr_tests = "test suite for typechecker on expressions" >::: [

    "built-in int_of_string" >::
    (fun _ -> assert_equal ~printer:show_typ
        (FuncTy (IntTy, StringTy))
        (typecheck_expr (parse_expr "int_of_string")));

    "built-in string_of_int" >::
    (fun _ -> assert_equal ~printer:show_typ
        (FuncTy (StringTy, IntTy))
        (typecheck_expr (parse_expr "string_of_int")));

    "built-in print_string" >::
    (fun _ -> assert_equal ~printer:show_typ
        (FuncTy (StringTy, UnitTy))
        (typecheck_expr (parse_expr "print_string")));

    "unit type" >::
    (fun _ -> assert_equal ~printer:show_typ
        (UnitTy)
        (typecheck_expr (parse_expr  "print_string \"hello\" ")));
    
    "add function" >::
    (fun _ -> assert_equal ~printer:show_typ
        (IntTy)
        (typecheck_expr (parse_expr "1+2")));

    "if expression (simple)" >::
    (fun _ -> assert_equal
        (BoolTy)
        (typecheck_expr (parse_expr "if 2 < 3 then true else false")));

    "tuple type" >::
    (fun _ -> assert_equal ~printer:show_typ
        (TupleTy ([IntTy; IntTy; IntTy]))
        (typecheck_expr (parse_expr "(1, 2, 3)")));

    "nested tuple type (left)" >::
    (fun _ -> assert_equal ~printer:show_typ
        (TupleTy [ TupleTy [IntTy; IntTy]; IntTy])
        (typecheck_expr (parse_expr "((1, 2), 3)")));

    "nested tuple type (right)" >::
    (fun _ -> assert_equal ~printer:show_typ
        (TupleTy [ IntTy; TupleTy [IntTy; IntTy]])
        (typecheck_expr (parse_expr "(1, (2, 3))")));

    "id function" >::
    (fun _ -> assert_equal ~printer:show_typ
        (FuncTy (IntTy, IntTy))
        (typecheck_expr (parse_expr "fun x : int => x")));

    "function expression, no type" >::
    (fun _ -> assert_equal
        (FuncTy(IntTy, FuncTy(IntTy, IntTy)))
        (typecheck_expr (parse_expr  "fun x y => x + 2 * y"))); 

    "function expression, with type" >::
    (fun _ -> assert_equal
        (FuncTy(IntTy, FuncTy(IntTy, IntTy)))
        (typecheck_expr (parse_expr  "fun x y : int => x + 2 * y"))); 

    "function expression, with incorrect type" >::
    (fun _ -> try
        let _ = typecheck_expr (parse_expr  "fun x y : bool => x + 2 * y") in
        assert_failure "'fun x y : bool => x + 2 * y' passed the typechecker"
    with
    | TypeError _ -> assert_bool "" true
    | _ -> assert_failure "Unexpected error");
    
    "function association: multiple arg" >::
    (fun _ -> assert_equal ~printer:show_typ
        (FuncTy (IntTy, FuncTy(IntTy, IntTy)))
        (typecheck_expr (parse_expr "fun x y : int => x + y")));

    "function association: return function" >::
    (fun _ -> assert_equal ~printer:show_typ
        (FuncTy (IntTy, FuncTy(IntTy, IntTy)))
        (typecheck_expr (parse_expr "fun (x : int) => (fun a : int => x + a)")));

    "match expr with given context (nested expr, pattern vars)" >::
    (fun _ -> assert_equal ~printer:show_typ
        (IntTy)
        (let e = parse_expr  "match p with | Pair (x, y) => x + y" in
        let st = TypeChecker.typecheck_expr e in
        TypeChecker.ConstrState.eval_state st  {clst = []; context = [("Pair", FuncTy(IntTy, FuncTy(IntTy, UserTy "pairing_type")))]}
        ));

    "let expr, no params, no type annotation" >::
    (fun _ -> assert_equal ~printer:show_typ
        (IntTy)
        (typecheck_expr (parse_expr "let x = 1 in x + 2")));

    "let expr, no params, with type annotation" >::
    (fun _ -> assert_equal ~printer:show_typ
        (IntTy)
        (typecheck_expr (parse_expr "let x : int = 1 in x + 2")));

    "application with let expr, with params, with type annotation" >::
    (fun _ -> assert_equal ~printer:show_typ
        (IntTy)
        (typecheck_expr (parse_expr "let f x y : int = x + 2 * y in f 1 2")));

    "function def with let expr" >::
    (fun _ -> assert_equal ~printer:show_typ
        (FuncTy(IntTy, (FuncTy (IntTy, IntTy))))
        (typecheck_expr (parse_expr "let f x y : int = x + 2 * y in f")));

    "application with let expr equals function" >::
    (fun _ -> assert_equal ~printer:show_typ
        (IntTy)
        (typecheck_expr (parse_expr "let f = fun x y : int => x + 2 * y in f 1 2")));

    "id function application from let expr" >::
    (fun _ -> assert_equal
        (TupleTy [IntTy; UnitTy])
        (typecheck_expr (parse_expr "let id x = x in (id 2, id ())")));

    "function with tuple type" >::
    (fun _ -> assert_equal ~printer:show_typ
        (parse_type "(int * int) -> (int * int)")
        (typecheck_expr (parse_expr " fun (p: int*int) => p")));

    "ill-typed" >::
    (fun _ -> try
        let _ = typecheck_expr (parse_expr "
        let f = fun (x:int) => (fun (y:bool) => ()) in f 3 ()") in
        assert_failure "'let f = fun (x:int) => (fun (y:bool) => ()) in f 3 ()' passed the typechecker"
    with
    | TypeError _ -> assert_bool "" true
    | _ -> assert_failure "Unexpected error");

    
]

let type_binding_tests = "test suite for typechecker on bindings" >::: [

    "let binding, with type" >::
    (fun _ -> assert_equal
        (FuncTy(IntTy, BoolTy))
        (typecheck_binding (List.hd (parse "let f (x : int) : bool = if x < 0 then true else false;;"))));

    "let binding, no type" >::
    (fun _ -> assert_equal
        (FuncTy(IntTy, BoolTy))
        (typecheck_binding (List.hd (parse "let f x = if x < 0 then true else false;;"))));

    "function application from let binding, no type" >::
    (fun _ -> assert_equal
        ([FuncTy(IntTy, BoolTy); BoolTy])
        (typecheck (parse
        "let f x = if x < 0 then true else false;;
        let result = f 1;;")));

    "type binding" >::
    (fun _ -> assert_equal
        (UserTy "pairing")
        (typecheck_binding (List.hd (parse "type pairing = | Pair of int * int;;"))));

    "user-defined type constructor" >::
    (fun _ -> assert_equal
        ([UserTy "pairing"; UserTy "pairing"])
        (typecheck (parse 
        "type pairing = | Pair of int * int;;
        let p = Pair (1, 2);;")));

    "match expression combined" >::
    (fun _ -> assert_equal
        ([UserTy "pairing"; UserTy "pairing"; IntTy])
        (typecheck (parse 
        "type pairing = | Pair of int * int | Single of int | Nothing ;;
        let p = Pair (1, 2);;
        let res = match p with | Pair (x, y) => x + y | Single x => x | Nothing => 0;;")));

    "match expression, multiple pvar" >::
    (fun _ -> assert_equal
        ([UserTy "pairing"; UserTy "pairing"; IntTy])
        (typecheck (parse 
        "type pairing = | Pair of int * int;;
        let p = Pair (1, 2);;
        let res = match p with | Pair (x, y) => x + y ;;")));

    "match expression, single pvar" >::
    (fun _ -> assert_equal
        ([UserTy "test"; UserTy "test"; IntTy])
        (typecheck (parse 
        "type test = | A of int | B of bool;;
        let a = A 1;;
        let res = match a with | A i => i | B j => 0 ;;")));

    "match expression, no pvar" >::
    (fun _ -> assert_equal
        ([UserTy "test"; UserTy "test"; BoolTy])
        (typecheck (parse 
        "type test = | A | B;;
        let p = A;;
        let res = match p with | A => true | B => false ;;")));

    "function currying: definition" >::
    (fun _ -> assert_equal
        (typecheck (parse 
        "let f x y = x + y;;"))
        (typecheck (parse 
        "let f = fun x => fun y => x + y;;")));

    "function currying: application" >::
    (fun _ -> assert_equal
        ([FuncTy(IntTy, FuncTy(IntTy, IntTy)); 
            FuncTy(IntTy, IntTy)])
        (typecheck (parse 
        "let f = fun x => fun y => x + y;;
        let g = f 3;;")));
        
]


let type_tests = "test_suite for typechecker" >::: [
    type_expr_tests;
    type_binding_tests;
  ]