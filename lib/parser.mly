%{
    open Ast
%}

%token Type             (** type - keyword *)
%token Of               (** of - keyword *)
%token Let              (** let - keyword *)
%token Rec              (** rec - keyword *)
%token In               (** in - keyword *)
%token If               (** if - keyword *)
%token Then             (** then - keyword *)
%token Else             (** else - keyword *)
%token Match            (** match - keyword *)
%token With             (** with - keyword *)
%token Fun              (** fun - keyword *)
%token True             (** true - keyword *)
%token False            (** false - keyword *)
%token Mod              (** mod - keyword *)
%token TInt             (** int - type name *)
%token TBool            (** bool - type name *)
%token TString          (** string - type name *)
%token TUnit            (** unit - type name *)
%token Eq               (** = - binary operator *)
%token Plus             (** + - binary operator *)
%token Minus            (** - - binary operator *)
%token Times            (** * - binary operator *)
%token Divide           (** / - binary operator *)
%token Lt               (** < - binary operator *)
%token Concat           (** ^ - binary operator *)
%token And              (** && - binary operator *)
%token Or               (** |%token - binary operator *)
%token Not              (** ! - unary operator *)
%token Negate           (** ~ - unary operator *)
%token DoubleSemicolon  (** ;; *)
%token Colon            (** : *)
%token Arrow            (** -> *)
%token DoubleArrow      (** => *)
%token LParen           (** ( *)
%token RParen           (** ) *)
%token Pipe             (** %token *)
%token Comma            (** , *)
%token <string> Id      (** Identifier, like a variable or function name *)
%token <int> Int        (** Integer literal *)
%token <string> String  (** String literal *)
%token EOF              (** End-of-file - you can ignore this *)


%left Or
%left And
%nonassoc Eq Lt
%left Plus Minus Concat
%left Times Divide Mod
%nonassoc Negate
%nonassoc Not

%start <program> start

%type <program> program
%type <binding> binding
%type <typ_binding> typ_binding
%type <typ_binding list> typ_bindings
%type <param> param
%type <param list> params require_params
%type <expr> expr application base
%type <expr list> expr_tuple
%type <typ> typ
%type <matchbranch> match_branch
%type <matchbranch list> match_branches
%type <pattern_vars> pattern_vars list_of_ids

%%

start:
  | p = program; EOF; { p } 

/* 
<program> ::= [<binding> ;;]+

<binding> ::= let $id [<param>]* [: <type>] = <expr>
            | let rec $id [<param>]* [: <type>] = <expr>
            | type $id = ['|' $id [of <type>]]+

<param> ::= $id
          | ( $id : <type> )
 */

program: 
  | b = binding; DoubleSemicolon;                 { [b] }
  | b = binding; DoubleSemicolon; bs = program    { b :: bs }

binding:
  | Let; x = Id; ps = params; Colon; t = typ;Eq; e = expr;          { LetB(x, ps, Some t, e)}
  | Let; x = Id; ps = params; Eq; e = expr;                   { LetB(x, ps, None, e )}
  | Let; Rec; x = Id; ps = params; Colon; t = typ; Eq; e = expr;     { LetRecB(x, ps, Some t, e)}
  | Let; Rec; x = Id; ps = params; Eq; e = expr;              { LetRecB(x, ps, None, e )}
  | Type; x = Id; Eq; ts = typ_bindings;                  { TypeB(x, ts)}

typ_bindings:
  | tb = typ_binding                      { [tb] }
  | tb = typ_binding; tbs = typ_bindings  { tb :: tbs }

typ_binding:
  | Pipe; x = Id               { (x, None)}
  | Pipe; x = Id; Of; t = typ  { (x, Some t)}

params:
  |                         { [] }
  | p = param; ps = params  { p:: ps } 

require_params:
  | p = param               { [p] }
  | p = param; ps = params  { p:: ps } 

param:
  | x = Id; Colon; t = typ  { { name = x; p_type = Some t } }
  | x = Id                  {  { name = x; p_type = None } }

/* 
<expr> ::= let $id [<param>]* [: <type>] = <expr> in <expr>
         | let rec $id [<param>]* [: <type>] = <expr> in <expr>
         | if <expr> then <expr> else <expr>
         | fun [<param>]+ [: <type>] => <expr>
         | <expr> <binop> <expr>
         | <unop> <expr>
         | ( <expr> [, <expr>]+ )
         | match <expr> with ['|' <match_branch>]+
         # | <expr> <expr>
         | <app>

<app> ::= <app> <base> | <base> # right-associative

<base> ::=          
         | ( <expr> )
         | $int
         | true
         | false
         | $string
         | $id
         | ( )

<binop> ::= + | - | * | / | mod | < | = | ^ | && | ||

<unop> ::= not | ~
 */

expr: 
  | Let; x = Id; ps = params; Colon; t = typ; Eq; e1 = expr; In; e2 = expr;          { LetExp(x, ps, Some t, e1, e2) }
  | Let; x = Id; ps = params; Eq ; e1 = expr; In; e2 = expr;                  { LetExp(x, ps, None, e1, e2) }
  | Let; Rec; x = Id; ps = params; Colon; t = typ; Eq; e1 = expr; In; e2 = expr;     { LetRecExp(x, ps, Some t, e1, e2) }
  | Let; Rec; x = Id; ps = params; Eq; e1 = expr; In; e2 = expr;              { LetRecExp(x, ps, None, e1, e2) }
  | If; e1 = expr; Then; e2 = expr; Else; e3 = expr;                          { If(e1,e2,e3) }
  | Fun; ps = require_params; Colon; t = typ; DoubleArrow; e = expr;                 { Fun(ps, Some t, e) }
  | Fun; ps = require_params; DoubleArrow; e = expr;                          { Fun(ps, None, e) }
  | e1 = expr; Plus; e2 = expr;                                               { Add(e1,e2) }
  | e1 = expr; Minus; e2 = expr;                                              { Sub(e1,e2) }
  | e1 = expr; Times; e2 = expr;                                              { Mul(e1,e2) }
  | e1 = expr; Divide; e2 = expr;                                             { Div(e1,e2) }
  | e1 = expr; Mod; e2 = expr;                                                { Modulo(e1,e2) }
  | e1 = expr; Lt; e2 = expr;                                                 { LessThan(e1,e2) }
  | e1 = expr; Eq; e2 = expr;                                                 { Equal(e1,e2) }
  | e1 = expr; Concat; e2 = expr;                                             { LogicAnd(e1,e2) }
  | e1 = expr; And; e2 = expr;                                                { LogicOr(e1,e2) }
  | e1 = expr; Or; e2 = expr;                                                 { StrConcat(e1,e2) }
  | Not; e = expr;                                                            { LogicNegate(e) }
  | Negate; e = expr;                                                         { IntNegate(e) } 
  | Match; e = expr; With; bs = match_branches;                               { MatchExp(e, bs) }
  | LParen; es = expr_tuple; RParen;                                          { Tuple(es) }
  | a = application;                                                          { a }

expr_tuple: 
  | e = expr;                           { [e] }
  | e = expr; Comma; es = expr_tuple;   { e:: es }

application:
  | a = application; b = base; { App(a, b) }
  | b = base;                  { b }

base:
  | LParen; e = expr; RParen;                                                 { e }
  | n = Int;                                                                  { CInt n }
  | True;                                                                     { CBool true }
  | False;                                                                    { CBool false }
  | s = String;                                                               { CString s }
  | x = Id;                                                                   { Var x }
  | LParen; RParen;                                                           { Unit }

/* 
<match_branch> ::= $id [<pattern_vars>] => <expr>

<pattern_vars> ::= $id
                 | ( $id [, $id ]+ )
 */

match_branches:
  | b = match_branch;                       { [b] }
  | b = match_branch; bs = match_branches;  { b:: bs }

match_branch: 
  | Pipe; x = Id; vs = pattern_vars; DoubleArrow; e = expr; { MatchBr(x, Some vs, e) }
  | Pipe; x = Id; DoubleArrow; e = expr;                    { MatchBr(x, None, e) }

pattern_vars:
  | x = Id;                                   { [x] }
  | LParen; x = Id; xs =list_of_ids; RParen;  { x::xs }

list_of_ids:
  | x = Id;                           { [x] }
  | x = Id; Comma; xs = list_of_ids   { x:: xs }
/* 
<type> ::= <type> -> <type>
         | ( <type> )
         | <type> * <type>
         | int
         | bool
         | string
         | unit
         | $id
*/

typ:
  | t1 = typ; Arrow; t2 = typ;  { FunTy (t1, t2) }
  | LParen; t = typ; RParen;    { t }
  | t1 = typ; Times; t2 = typ;  { TupleTy (t1,t2) }
  | TInt;                       { IntTy }
  | TBool;                      { BoolTy }
  | TString;                    { StringTy }
  | TUnit;                      { UnitTy }
  | x = Id;                     { CustomTy x }

/* 
program:
  | t = term;                 { t }
  | b = binding; p = program; { let (n, v) = b in EApp (ELambda (n, p), v) }

binding:
  | id = Var; Equal; t = term; Semicolon; { (id, t) }

term:
  | Lambda; id = Var; Dot; t = term; { ELambda (id, t) }
  | a = application;                 { a }

application:
  | a = application; b = base; { EApp (a, b) }
  | b = base;                  { b }

base:
  | id = Var;                 { EVar id }
  | LParen; t = term; RParen; { t } */
