open OUnit2
open Ocaml_lite.Lexer
open Ocaml_lite.Ast

let parse_expr_tests = "test suite for parser (expr helper)" >::: [
    "tuple" >::
    (fun _ -> assert_equal
        (Tuple([CInt 1;CInt 2;CInt 3]))
        (parse_expr "(1, 2, 3)"));

    "arithmetic computations" >::
    (fun _ -> assert_equal
        (
            Binop(Binop(
                Binop(
                    Binop(CInt 1, Div, CInt 2),
                    Add, Binop(CInt 3, Mul, CInt 4)), 
                Sub, CInt 4), 
            Modulo, CInt 6)

        )
        (parse_expr "(1 / 2 + 3 * 4 - 4 ) mod 6"));

    "comparisons" >::
    (fun _ -> assert_equal
        (Binop(
            Binop(Binop(CInt 2, Add, CInt 3),
            LessThan, CInt 7), 
            Equal, CBool true
        ))
        (parse_expr "(2 + 3 < 7) = true"));

    "logic expressions" >::
    (fun _ -> assert_equal
        (Binop(
            Binop(
                Unop(LogicNegate, CBool true),
                LogicAnd, CBool false),
            LogicOr, CBool true
        ))
        (parse_expr "not true && false || true"));

    "string concat" >::
    (fun _ -> assert_equal
        (Binop(CString "hello", StrConcat, CString "world"))
        (parse_expr "\"hello\" ^ \"world\""));

    "integer negate" >::
    (fun _ -> assert_equal
        (Binop(Unop(IntNegate , CInt 1), Add, CInt 2))
        (parse_expr "~1 + 2"));

    "if expression (simple)" >::
        (fun _ -> assert_equal
            (IfExp (Binop(CInt 2, LessThan, CInt 3), CBool true, CBool false))
            (parse_expr  "if 2 < 3 then true else false"));

    "if expression (nested expr) " >::
        (fun _ -> assert_equal
            (IfExp (Binop(CInt 2, LessThan, CInt 3), Binop(CInt 1, Add, CInt 2), Binop(CInt 2, Add, CInt 2)))
            (parse_expr  "if 2 < 3 then 1+2 else 2+2"));

    "let expression, with type" >::
        (fun _ -> assert_equal
            (LetExp("x", false, [], Some IntTy, CInt 0, Binop(Var "x", Add, CInt 1)))
            (parse_expr "let x : int = 0 in x + 1"));

    "let expression, no type" >::
    (fun _ -> assert_equal
        (LetExp("x", false, [], None, CInt 0, Binop(Var "x", Add, CInt 1)))
        (parse_expr  "let x = 0 in x + 1"));

    "let rec expression, no type" >::
    (fun _ -> assert_equal
        (LetExp("x", true, [], None, CInt 0, Binop(Var "x", Add, CInt 1)))
        (parse_expr  "let rec x = 0 in x + 1"));

    "match expression (simple)" >::
    (fun _ -> assert_equal
        (MatchExp(Var "p", [
            MatchBr("Pair", None, Var "p")]))
        (parse_expr  "match p with | Pair => p"));

    "match expression (nested expr, pattern vars)" >::
    (fun _ -> assert_equal
        (MatchExp(Var "p", [
            MatchBr("Pair", Some ["x"; "y"], Binop(Var "x", Add, Var "y"))]))
        (parse_expr  "match p with | Pair (x, y) => x + y"));

    "nested match expression" >::
    (fun _ -> assert_equal
        (parse_expr  "match p with | Pair (x, y) => (match x with | Odd => 1 | Even => ~1)")
        (parse_expr  "match p with | Pair (x, y) =>  match x with | Odd => 1 | Even => ~1 "));

    "function expression, no type" >::
    (fun _ -> assert_equal
        (Function([{ name="x"; p_type=None }; { name="y"; p_type=None }], Some IntTy, 
        Binop(Var "x", Add, Binop(CInt 2, Mul, Var "y"))))
        (parse_expr  "fun x y : int => x + 2 * y"));

    "function expression, with type" >::
    (fun _ -> assert_equal
        (Function([{ name="x"; p_type=Some IntTy}; { name="y"; p_type=Some IntTy}], Some IntTy, 
        Binop(Var "x", Add, Binop(CInt 2, Mul, Var "y"))))
        (parse_expr  "fun (x:int) (y:int) : int => x + 2 * y"));

    "function application from helper let expression, no type" >::
    (fun _ -> assert_equal
        (LetExp("f", false, [{ name="x"; p_type=Some IntTy}], Some BoolTy,
        IfExp (Binop(Var "x", LessThan, CInt 0), CBool true, CBool false),
        App (Var "f", CInt 1)))
        (parse_expr
            "let f (x : int) : bool = if x < 0 then true else false in f 1"));
]

let parse_binding_tests = "test suite for parser (top level bindings)" >::: [
    "unit" >::
    (fun _ -> assert_equal
        ([LetB("res", false, [], None, Unit)]
        )
        (parse "let res = ();;"));

    "let binding, with type" >::
    (fun _ -> assert_equal
        (
          [LetB("f", false, [{ name="x"; p_type=Some IntTy}], Some BoolTy, 
          IfExp (Binop(Var "x", LessThan, CInt 0), CBool true, CBool false))]
        )
        (parse
            "let f (x : int) : bool = if x < 0 then true else false;;"));

    "function application from let binding, no type" >::
    (fun _ -> assert_equal
        ([LetB("f", false, [{ name="x"; p_type=None}], None, 
            IfExp (Binop(Var "x", LessThan, CInt 0), CBool true, CBool false));
         LetB("result", false, [], None, App (Var "f", CInt 1))
        ])
        (parse
            "let f x = if x < 0 then true else false;;
            let result = f 1;;"));

    "function application from let binding, with type" >::
    (fun _ -> assert_equal
        ([
          LetB("f", false, [{ name="x"; p_type=Some IntTy}], Some BoolTy, 
          IfExp (Binop(Var "x", LessThan, CInt 0), CBool true, CBool false));
          LetB("result", false, [], None, App (Var "f", CInt 1))
        ])
        (parse
            "let f (x : int) : bool = if x < 0 then true else false;;
            let result = f 1;;"));

    "type binding" >::
    (fun _ -> assert_equal
        ([TypeB("pairing", [
            ("Pair", Some (TupleTy [IntTy; IntTy]))
        ])])
        (parse "type pairing = | Pair of int * int;;"));
  ]

let parse_typ_tests = "test suite for parser (top level bindings)" >::: [
    "tuple type" >::
    (fun _ -> assert_equal ~printer:show_typ
        (TupleTy [IntTy; IntTy; IntTy])
        (parse_type "int * int * int"));

    "nested tuple type (left)" >::
    (fun _ -> assert_equal ~printer:show_typ
        (TupleTy [TupleTy [IntTy; IntTy]; IntTy])
        (parse_type "(int * int) * int"));

    "nested tuple type (right)" >::
    (fun _ -> assert_equal ~printer:show_typ
        (TupleTy [ IntTy; TupleTy [IntTy; IntTy]])
        (parse_type "int * (int * int)"));

    "function with tuple type" >::
    (fun _ -> assert_equal ~printer:show_typ
        (parse_type "(int * int) -> (int * int)")
        (parse_type "int * int -> int * int"));

    "function association" >::
    (fun _ -> assert_equal ~printer:show_typ
        (FuncTy(IntTy, FuncTy(IntTy, IntTy)))
        (parse_type "int -> int -> int"));
]

let parse_tests = "test_suite for parser" >::: [
    parse_expr_tests;
    parse_binding_tests;
    parse_typ_tests;
  ]