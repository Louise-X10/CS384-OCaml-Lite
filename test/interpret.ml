open OUnit2
(* open Ocaml_lite.Lexer
open Ocaml_lite.Ast *)


let interp_tests = "test suite for interpretor" >::: [
  (* 
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
   *)  
  ]

 let interp_tests = "test_suite for interpretor" >::: [
  (* interpret_tests; *)
]
