open OUnit2
open Ocaml_lite.Lexer
open Ocaml_lite.Ast
open Ocaml_lite.Parser
open Ocaml_lite.Typechecker

let lex_tests = "test suite for tokenize" >::: [
    "random code" >::
    (fun _ -> assert_equal
        [Let; Id "f"; LParen; Id "x"; Colon; TInt; RParen; Colon; TString;
         Eq; If; Id "x"; Lt; Int 0; Then; String "neg"; Else; String "pos"]
        (tokenize
           "let f (x : int) : string = if x < 0 then \"neg\" else \"pos\""));

    "all tokens" >::
    (fun _ -> assert_equal
      [Let; Rec; If; Then; Else; Fun; True; False; Mod; TInt; TBool; TString;
       TUnit; Eq; Plus; Minus; Times; Divide; Lt; Concat; And; Or; Not;
       Negate; DoubleSemicolon; Colon; Arrow; LParen; RParen;
       Id "function_name"; Int 32; String "str"; Pipe; DoubleArrow]
      (tokenize ("let rec if then else fun true false mod int bool string " ^
                 "unit =+-*/<^ && || not ~;;: -> () function_name 32 " ^
                 "\"str\"|=>")));

    "underscore id" >::
    (fun _ -> assert_equal [Id "_x32"] (tokenize "_x32"));

    "number id" >::
    (fun _ -> assert_equal [Int 32; Id "xyz"] (tokenize "32xyz"));
  ]

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

let parse_tests = "test suite for parser (top level bindings)" >::: [
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
]

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
    
    "add function association" >::
    (fun _ -> assert_equal ~printer:show_typ
        (FuncTy (IntTy, FuncTy(IntTy, IntTy)))
        (typecheck_expr (parse_expr "fun x y : int => x + y")));

    "match expr (nested expr, pattern vars)" >::
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

    "application with let expr equals function" >::
    (fun _ -> assert_equal ~printer:show_typ
        (IntTy)
        (typecheck_expr (parse_expr "let f = fun x y : int => x + 2 * y in f 1 2")));

   (*  "application convoluted" >::
    (fun _ -> assert_equal ~printer:show_typ
        (FuncTy( FuncTy(IntTy, IntTy), FuncTy(IntTy, IntTy) ))
        (typecheck_expr (parse_expr "
        let f = fun x : int => 2 * x in
        fun f => (fun a : int => (f a))"))); *)
]

(* let type_tests = "test suite for typechecker" >::: [
    
    "function with tuple type" >::
    (fun _ -> assert_equal ~printer:show_typ
        (typecheck (parse "(int * int) -> (int * int）"))
        (typecheck (parse "fun (x, y) => (x+1, y+1)")));

    "ill-typed" >::
        (fun _ -> try
            let _ = typecheck (parse "
            let f = fun (x:int) => (fun (y:bool) => ()) in f 3 ()") in
            assert_failure "'let f = fun (x:int) => (fun (y:bool) => ()) in f 3 ()' passed the typechecker"
        with
        | TypeError _ -> assert_bool "" true
        | _ -> assert_failure "Unexpected error");

    "apply inside lambda" >::
        (fun _ -> try
            let _ = typecheck (parse "fun x => x y") in
            assert_failure "'fun x => x y' passed the typechecker"
        with
        | TypeError _ -> assert_bool "" true
        | _ -> assert_failure "Unexpected error");

    "match expression" >::
    (fun _ -> assert_equal ~printer:show_typ
        (IntTy)
        (typecheck_expr (parse_expr "match p with | Pair => 0")));
  ]

let interp_tests = "test suite for interpretor" >::: [
  "arithmetic computations" >::
  (fun _ -> assert_equal
      (CInt 2)
      (interpret "(1 / 2 + 3 * 4 - 4 ) mod 6"));

  "comparisons" >::
  (fun _ -> assert_equal
      (CBool true)
      (interpret "2 + 3 < 7 = true"));

  "logic expressions" >::
  (fun _ -> assert_equal
      (CBool true)
      (interpret "not true && false || true"));

  "string concat" >::
  (fun _ -> assert_equal
      (CString "helloworld")
      (interpret " \"hello\" ^ \"world\" "));

  "integer negate" >::
  (fun _ -> assert_equal
      (CInt 1)
      (interpret  "~1 + 2"));

  "if expression" >::
  (fun _ -> assert_equal
      (CBool true)
      (interpret  "if 2 < 3 then true else false"));

  "let expression, with type" >::
  (fun _ -> assert_equal
      (CInt 1)
      (interpret  "let x : int = 0 in x + 1"));

  "let expression, no type" >::
  (fun _ -> assert_equal
      (CInt 1)
      (interpret  "let x = 0 in x + 1"));

  "let binding, with type" >::
  (fun _ -> assert_equal
      (
        LetB("f", [{ name="x"; p_type=None}], Some BoolTy, 
        IfExp (LessThan(Var "x", CInt 0), CBool true, CBool false))
      )
      (interpret
          "let f (x : int) : bool = if x < 0 then true else false;;"));

  "function application from let binding, no type" >::
  (fun _ -> assert_equal
      ([
        LetB("f", [{ name="x"; p_type=Some IntTy}], Some BoolTy, 
        IfExp (LessThan(Var "x", CInt 0), CBool true, CBool false)),
        LetB("result", [], Some BoolTy, CBool true)
      ])
      (interpret
          "let f x = if x < 0 then true else false;;
          let result = f 1;;"));

    "function application from let binding, with type" >::
    (fun _ -> assert_equal
        ([
        LetB("f", [{ name="x"; p_type=Some IntTy}], Some BoolTy, 
        IfExp (LessThan(Var "x", CInt 0), CBool true, CBool false)),
        LetB("result", [], Some BoolTy, CBool true)
        ])
        (interpret
            "let f (x : int) : bool = if x < 0 then true else false;;
                let result = f 1;;"));

    "function application from helper let expression, with type" >::
    (fun _ -> assert_equal
        (CBool true)
        (interpret
            "let f (x : int) : bool = if x < 0 then true else false in f 1"));

  "match expression" >::
  (fun _ -> assert_equal
      ([
        TypeB("pairing", ["Pair", Some (TupleTy [IntTy; IntTy])]),
        LetB("p", [], Some (UserTy "pairing"), App( Var "Pair", Tup ([CInt 1, CInt 2]))),
        LetB("result", [], Some IntTy, CInt 3)
      ])
      (interpret  "
      type pairing = | Pair of int * int;;
      let p : pairing = Pair (1, 2);;
      let result = match p with | Pair (x, y) => x + y;;"));

"nested match expression" >::
(fun _ -> assert_equal
    ([
    TypeB("pairing", ["Pair", Some (TupleTy [IntTy; IntTy])]),
    LetB("p", [], Some (UserTy "pairing"), App( Var "Pair", Tup ([CInt 1, CInt 2]))),
    LetB("result", [], Some IntTy, CInt 1)
    ])
    (interpret  "
    type pairing = | Pair of int * int;;
    let p : pairing = Pair (1, 2);;
    let result = match p with Pair (x, y) => match x with | 1 => 1 | 0 => -1;;"));

  "function expression, no type" >::
  (fun _ -> assert_equal
      (
      Function([{ name="x"; p_type=Some IntTy}; { name="y"; p_type=Some IntTy}], Some IntTy, 
      Add(Var "x", Mul(CInt 2, Var "y")))  
      )
      (interpret  "fun x y : int => x + 2 * y"));

  "function expression, with type" >::
  (fun _ -> assert_equal
      (
      Function([{ name="x"; p_type=Some IntTy}; { name="y"; p_type=Some IntTy}], Some IntTy, 
      Add(Var "x", Mul(CInt 2, Var "y")))  
      )
      (interpret  "fun (x:int) (y:int) : int => x + 2 * y"));
    
  ]
 *)
let tests = "test_suite for ocaml-lite" >::: [
    lex_tests;
    parse_expr_tests;
    parse_tests;
    parse_typ_tests;
    type_expr_tests;
    (* type_tests; *)
    (* interpret_tests; *)
  ]

let _ = run_test_tt_main tests
