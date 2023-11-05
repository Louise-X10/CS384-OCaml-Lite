open OUnit2
open Ocaml_lite.Lexer
open Ocaml_lite.Ast
open Ocaml_lite.Parser
(* open Ocaml_lite.Typechecker *)

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
        (Modulo(
            Sub(
                Add(
                Div(CInt 1, CInt 2), 
                Mul(CInt 3, CInt 4)),
            CInt 4),
        CInt 6)
        )
        (parse_expr "(1 / 2 + 3 * 4 - 4 ) mod 6"));

    "comparisons" >::
    (fun _ -> assert_equal
        (Equal(LessThan(Add(CInt 2, CInt 3), CInt 7), CBool true))
        (parse_expr "(2 + 3 < 7) = true"));

    "logic expressions" >::
    (fun _ -> assert_equal
        (LogicOr(LogicAnd(LogicNegate(CBool true), CBool false), CBool true))
        (parse_expr "not true && false || true"));

    "string concat" >::
    (fun _ -> assert_equal
        (StrConcat(CString "hello", CString "world"))
        (parse_expr "\"hello\" ^ \"world\""));

    "integer negate" >::
    (fun _ -> assert_equal
        (Add(IntNegate (CInt 1), CInt 2))
        (parse_expr "~1 + 2"));

    "if expression (simple)" >::
        (fun _ -> assert_equal
            (IfExp (LessThan(CInt 2, CInt 3), CBool true, CBool false))
            (parse_expr  "if 2 < 3 then true else false"));

    "if expression (nested expr) " >::
        (fun _ -> assert_equal
            (IfExp (LessThan(CInt 2, CInt 3), Add(CInt 1, CInt 2), Add(CInt 2, CInt 2)))
            (parse_expr  "if 2 < 3 then 1+2 else 2+2"));

    "let expression, with type" >::
        (fun _ -> assert_equal
            (LetExp("x", false, [], Some IntTy, CInt 0, Add(Var "x", CInt 1)))
            (parse_expr "let x : int = 0 in x + 1"));

    "let expression, no type" >::
    (fun _ -> assert_equal
        (LetExp("x", false, [], None, CInt 0, Add(Var "x", CInt 1)))
        (parse_expr  "let x = 0 in x + 1"));

    "let rec expression, no type" >::
    (fun _ -> assert_equal
        (LetExp("x", true, [], None, CInt 0, Add(Var "x", CInt 1)))
        (parse_expr  "let rec x = 0 in x + 1"));

    "match expression (simple)" >::
    (fun _ -> assert_equal
        (MatchExp(Var "p", [
            MatchBr("Pair", None, Var "p")]))
        (parse_expr  "match p with | Pair => p"));

    "match expression (nested expr)" >::
    (fun _ -> assert_equal
        (MatchExp(Var "p", [
            MatchBr("Pair", Some ["x"; "y"], Add (Var "x", Var "y"))]))
        (parse_expr  "match p with | Pair (x, y) => x + y"));

    "nested match expression" >::
    (fun _ -> assert_equal
        (parse_expr  "match p with | Pair (x, y) => (match x with | Odd => 1 | Even => ~1)")
        (parse_expr  "match p with | Pair (x, y) =>  match x with | Odd => 1 | Even => ~1 "));

    "function expression, no type" >::
    (fun _ -> assert_equal
        (Function([{ name="x"; p_type=None }; { name="y"; p_type=None }], Some IntTy, 
        Add(Var "x", Mul(CInt 2, Var "y"))))
        (parse_expr  "fun x y : int => x + 2 * y"));

    "function expression, with type" >::
    (fun _ -> assert_equal
        (Function([{ name="x"; p_type=Some IntTy}; { name="y"; p_type=Some IntTy}], Some IntTy, 
        Add(Var "x", Mul(CInt 2, Var "y"))))
        (parse_expr  "fun (x:int) (y:int) : int => x + 2 * y"));

    "function application from helper let expression, no type" >::
    (fun _ -> assert_equal
        (LetExp("f", false, [{ name="x"; p_type=Some IntTy}], Some BoolTy,
        IfExp (LessThan(Var "x", CInt 0), CBool true, CBool false),
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
          IfExp (LessThan(Var "x", CInt 0), CBool true, CBool false))]
        )
        (parse
            "let f (x : int) : bool = if x < 0 then true else false;;"));

    "function application from let binding, no type" >::
    (fun _ -> assert_equal
        ([LetB("f", false, [{ name="x"; p_type=None}], None, 
            IfExp (LessThan(Var "x", CInt 0), CBool true, CBool false));
         LetB("result", false, [], None, App (Var "f", CInt 1))
        ])
        (parse
            "let f x = if x < 0 then true else false;;
            let result = f 1;;"));

    
    "function application from let binding, with type" >::
    (fun _ -> assert_equal
        ([
          LetB("f", false, [{ name="x"; p_type=Some IntTy}], Some BoolTy, 
          IfExp (LessThan(Var "x", CInt 0), CBool true, CBool false));
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

(* let type_tests = "test suite for typechecker" >::: [

  "built-in int_of_string" >::
  (fun _ -> assert_equal ~printer:tok_to_str
      (FuncTy (IntTy, StringTy))
      (typecheck (parse "int_of_string")));

  "built-in string_of_int" >::
  (fun _ -> assert_equal ~printer:tok_to_str
      (FuncTy (StringTy, IntTy))
      (typecheck (parse "string_of_int")));

  "built-in print_string" >::
  (fun _ -> assert_equal ~printer:tok_to_str
      (FuncTy (StringTy, UnitTy))
      (typecheck (parse "print_string")));

    "tuple type" >::
  (fun _ -> assert_equal ~printer:tok_to_str
      (TupleTy [IntTy; IntTy; IntTy])
      (typecheck (parse "int * int * int")));

    "nested tuple type (left)" >::
    (fun _ -> assert_equal ~printer:tok_to_str
        (TupleTy [ TupleTy [IntTy; IntTy]; IntTy])
        (typecheck (parse "(int * int) * int")));

    "nested tuple type (right)" >::
    (fun _ -> assert_equal ~printer:tok_to_str
        (TupleTy [ IntTy; TupleTy [IntTy; IntTy]])
        (typecheck (parse "int * (int * int）")));

  "id function" >::
  (fun _ -> assert_equal ~printer:tok_to_str
      (FuncTy (IntTy, IntTy))
      (typecheck (parse "fun x : int => x")));

  "add function" >::
  (fun _ -> assert_equal ~printer:tok_to_str
      (FuncTy (TupleTy [IntTy; IntTy], IntTy))
      (typecheck (parse "fun x y : int => x + y")));

  "application" >::
  (fun _ -> assert_equal ~printer:tok_to_str
      (IntTy)
      (typecheck (parse "let f = fun x y : int => x + 2y in f 1 2")));

  "application convoluted" >::
      (fun _ -> assert_equal ~printer:tok_to_str
          (FuncTy( FuncTy(IntTy, IntTy), FuncTy(IntTy, IntTy) ))
          (typecheck (parse "
          let f = fun x : int => 2x in
          fun f => (fun a : int => (f a))")));

  "unit type" >::
      (fun _ -> assert_equal ~printer:tok_to_str
          (UnitTy)
          (typecheck (parse "print_string \"hello\" ")));

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
        LetB("p", [], Some (CustomTy "pairing"), App( Var "Pair", Tup ([CInt 1, CInt 2]))),
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
    LetB("p", [], Some (CustomTy "pairing"), App( Var "Pair", Tup ([CInt 1, CInt 2]))),
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
      (interpret  "fun x y : int => x + 2y"));

  "function expression, with type" >::
  (fun _ -> assert_equal
      (
      Function([{ name="x"; p_type=Some IntTy}; { name="y"; p_type=Some IntTy}], Some IntTy, 
      Add(Var "x", Mul(CInt 2, Var "y")))  
      )
      (interpret  "fun (x:int) (y:int) : int => x + 2y"));
    
  ]
 *)
let tests = "test_suite for ocaml-lite" >::: [
    lex_tests;
    parse_expr_tests;
    parse_tests;
    (* type_tests; *)
    (* interpret_tests; *)
  ]

let _ = run_test_tt_main tests
