type token =
  | Id of string;;  

type tok_list =
  | Nil
  | Cons of token * tok_list ;;

type expr =
  | Var of string
  | App of expr * expr
  | Error ;;

type pair =
  | Pair of expr * tok_list ;;

let rec expr_to_string (ex : expr) : string = match ex with
  | Var id => "Var " ^ id
  | App (e1, e2) => "App (" ^ expr_to_string e1 ^ ", " ^ expr_to_string e2 ^ ")"
  | Error => "Error" ;;

let rec has_error (ex : expr) : bool = match ex with
  | Var id => false
  | App (e1, e2) => has_error e1 || has_error e2
  | Error => true ;;

type parse_mode =
  | Expr
  | Aexpr
  | Item ;;

let rec parse_help (mode : parse_mode) (src : tok_list) : pair =

  let expr (src : tok_list) : pair = match src with
    | Nil => parse_help Aexpr src
    | Cons (hd, tl) => (match hd with
      | Id id => parse_help Aexpr src) in

  let aexpr (src : tok_list) : pair =
    let rec helper ex src = match src with
      | Nil => Pair (ex, src)
      | Cons (hd, tl) => (match hd with
        | Id id => (match parse_help Item src with
          | Pair (i, r) => helper (App (ex, i)) r)) in
    match parse_help Item src with
    | Pair (e1, r) => helper e1 r in

  let item (src : tok_list) : pair = match src with
    | Nil => Pair (Error, Nil)
    | Cons (hd, tl) => (match hd with
      | Id id => Pair (Var id, tl)) in

  match mode with
  | Expr => expr src
  | Aexpr => aexpr src
  | Item => item src ;;

let parse_expr (src : tok_list) : expr =
  match parse_help Expr src with
  | Pair (ex, rest) => (match rest with
    | Nil => if has_error ex then Error else ex
    | Cons (hd, tl) => Error) ;;

let source : tok_list =
  Cons (Id "x", Cons (Id "y", Nil));;


let ast : expr = parse_expr source ;;
let print : unit = print_string (expr_to_string ast) ;;